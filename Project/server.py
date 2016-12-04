#!/usr/bin/env python 

# CS131 Python Project 

from __future__ import print_function
from twisted.application import service, internet
from twisted.internet import reactor, protocol
from twisted.protocols.basic import LineReceiver
from twisted.python import log 
from twisted.web.client import getPage 

import json 
import logging 
import sys 
import time 

GOOGLE_PLACES_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
GOOGLE_PLACES_KEY = "AIzaSyBKqQMeqi7_y1M0DCl2zgdo537BseT0gAk"


#####################################
### Server connections look like  ###
### Alford ---- Hamilton 		  ###
###   |				\			  ###
###   |				 \			  ###
###	  |				Holiday		  ###
###   |				 /   		  ###
###   |				/			  ###
### Welsh ------ Ball 			  ###
#####################################

SERVER_NEIGHBORS = {
	'Alford': ['Hamilton', 'Welsh'],
	'Ball': ['Holiday', 'Welsh'], 
	'Hamilton': ['Alford', 'Holiday'],
	'Holiday': ['Ball', 'Hamilton'],
	'Welsh': ['Alford', 'Ball']
}

SERVER_PORTS = {
	'Alford': 12540, 
	'Ball': 12541, 
	'Hamilton': 12542, 
	'Holiday': 12543,
	'Welsh': 12544
}

def usage(): 
	print ("Usage: [python] server.py [SERVER_NAME]\nValid server names are Alford, Ball, Hamilton, Holiday, Welsh")

# Client code
class TwistedHerdClient(protocol.ClientFactory):
	def __init__(self, msg):
		self.message = msg 

	def buildProtocol(self, addr):
		return TwistedHerdClientProtocol(self)

class TwistedHerdClientProtocol(LineReceiver):
	def __init__(self, factory):
		self.factory = factory

	def connectionMade(self):
		self.sendLine(self.factory.message)

# Server code 
class TwistedHerdServer(protocol.ServerFactory):
	def __init__(self, server_name): 
		self.server_name = server_name
		self.clients = {}
		self.num_connections = 0
		logging.basicConfig(filename="{}.log".format(self.server_name), level=logging.DEBUG)
		logging.info("Initialized Server at time {}: {} ".format(time.time(), self.server_name))

	def buildProtocol(self, addr): 
		return TwistedHerdServerProtocol(self)

class TwistedHerdServerProtocol(LineReceiver): 
	def __init__(self, server): 
		self.factory = server 

	# Functions that track number of connections 
	def connectionMade(self): 
		self.factory.num_connections += 1
		logging.info("Established connection: The total number of connections is {}".format(self.factory.num_connections))

	def connectionLost(self, reason):
		self.factory.num_connections -= 1
		logging.info("Lost connection: The total number of connections is {}".format(self.factory.num_connections))

	# Functions that handle commands
	def respondToInvalidCommand(self, command, msg="Invalid command"):
		logging.info("{}: {}".format(msg, command))
		self.transport.write("? {}\n".format(command))

	def lineReceived(self, line):
		logging.info("Command received: {}".format(line))

		if not line:
			self.respondToInvalidCommand(line)
			return 

		command = line.split()

		if command[0] == "AT":
			self.respondToAT(line)
		elif command[0] == "IAMAT":
			self.respondToIAMAT(line)
		elif command[0] == "WHATSAT":
			self.respondToWHATSAT(line)
		else:
			self.respondToInvalidCommand(line)
			return 

	# Server-to-server communication 
	def respondToAT(self, command): 
		args = command.split()

		if len(args) != 6: 
			self.respondToInvalidCommand(command, "Argument length error")
			return 

		server_name = args[1]
		client_id = args[3]
		client_send_time = args[5]

		if (client_id in self.factory.clients) and (client_send_time < self.factory.clients[client_id]['client_send_time']):
			logging.info("Received duplicate or stale information: {} to {}".format(server_name, self.factory.server_name))
			return

		self.factory.clients[client_id] = {'response': command, 'client_send_time': client_send_time}
		logging.info("Client {} updated".format(client_id))
		self.propagateUpdate(command, server_name)

	# Respond to client with an AT message 
	def respondToIAMAT(self, command): 
		args = command.split()
		if len(args) != 4: 
			self.respondToInvalidCommand(command, "Argument length error")
			return 

		client_id = args[1]
		client_coords = args[2] 
		client_send_time = (args[3])

		try: 
			time_difference = time.time() - float(client_send_time)
		except Exception as e:
			self.respondToInvalidCommand(command, "Argument type error")
			return 

		if time_difference > 0:
			response = "AT {} +{} {}".format(self.factory.server_name, time_difference, ' '.join(args[1:]))
		else:
			response = "AT {} {} {}".format(self.factory.server_name, time_difference, ' '.join(args[1:]))

		self.transport.write(response + '\n')
		logging.info("Responded to IAMAT: {}".format(response))

		if (client_id in self.factory.clients) and (client_send_time < self.factory.clients[client_id]['client_send_time']):
			logging.info("Received duplicate or stale information: {} to {}".format(server_name, self.factory.server_name))
			return

		self.factory.clients[client_id] = {'response': response, 'client_send_time': client_send_time}
		logging.info("Client {} updated".format(client_id))
		self.propagateUpdate(response)

	# Consult Google Places API to generate results for client query
	def respondToWHATSAT(self, command): 
		args = command.split()
		if len(args) != 4: 
			self.respondToInvalidCommand(command, "Argument length error")
			return 

		client_id = args[1]  
		try: 
			search_radius = int(args[2])
			num_responses = int(args[3])
		except Exception as e: 
			self.respondToInvalidCommand(command, "Argument type error") 
			return 

		if (search_radius > 50) or (num_responses > 20):
			self.respondToInvalidCommand(command, "Argument out of range")
			return 

		if not (client_id in self.factory.clients):
			self.respondToInvalidCommand(command, "Unknown client")
			return 

		# Search for client in server list 
		client_info = self.factory.clients[client_id]['response']
		# response was stored as 
		# AT [Server] [Time diff] [Client] [Coords] [Send time]
		client_coords = client_info.split()[4]

		try: 
			coords = client_coords.replace('-', ',-')
			query_url = "{}location={}&radius={}&key={}".format(GOOGLE_PLACES_URL, coords, search_radius, GOOGLE_PLACES_KEY)
			logging.info("Querying Google Places with: {}.".format(query_url))
			query_response = getPage(query_url)

			# This ensures that when data is fetched, callback will execute
			query_response.addCallback(callback = lambda x: (self.respondWithQueryResults(x, client_info, num_responses, query_url)))
		except Exception as e:
			logging.error("Query to Google Places failed: Try again or check arguments are valid")


	def propagateUpdate(self, message, seen=''):
		for s in SERVER_NEIGHBORS[self.factory.server_name]:
			if s != seen:
				reactor.connectTCP('localhost', SERVER_PORTS[s], TwistedHerdClient(message))
				logging.info("Flooding message: {} to {}".format(self.factory.server_name, SERVER_PORTS[s]))

	def respondWithQueryResults(self, data, client_info, num_responses, client_id):
		response = json.loads(data)
		results = response['results']
		# Chunk off only the responses requested by user 
		response['results'] = results[0:num_responses]
		self.transport.write("{}\n{}\n\n".format(client_info, json.dumps(response, indent=4)))
		logging.info("Responded to WHATSAT: {}".format(client_info))

def main(): 
	if len(sys.argv) != 2:
		usage()
		exit()

	server_name = sys.argv[1]
	try: 
		if server_name in SERVER_NEIGHBORS:
			factory = TwistedHerdServer(server_name)
			reactor.listenTCP(SERVER_PORTS[server_name], factory)
			reactor.run()
		else: 
			usage()
	except KeyError: 
		print("Error in configuration")

# Ensures module is only run if not imported 
if __name__ == '__main__':
	main()
