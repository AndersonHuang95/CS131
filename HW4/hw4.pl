/* Morse code representation */ 
morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)

/* Shorter signals come first */ 
signal([.], A, B) :- A =:= 1, B =:= 1. 			% dih 
signal([-], A, B) :- A >= 3, B =:= 1.			% dah 
signal([.], A, B) :- A =:= 2, B =:= 1.			% ambiguous dih 
signal([-], A, B) :- A =:= 2, B =:= 1.			% ambiguous dah

signal([], A, B) :- A =:= 1, B =:= 0. 			% space b/t dih, dah
signal([], A, B) :- A =:= 2, B =:= 0. 			% ambiguous space b/t dih, dah
signal([^], A, B) :- A =:= 2, B =:= 0. 			% ambiguous space b/t letter
signal([^], A, B) :- A =:= 3, B =:= 0. 			% space b/t letters
signal([^], A, B) :- A =:= 4, B =:= 0.			% ambiguous space b/t letters
signal([^], A, B) :- A =:= 5, B =:= 0. 			% ambiguous space b/t letters	
signal([#], A, B) :- A > 5, B =:= 0. 			% space b/t words 

/* RLE encoding */
/* Reverse, then find longest consecutive sequence */ 
/* Reverse back, repeat until empty */
encode([], []).
encode(List, [E | ETail]) :-
    reverse(List, ReversedList),
    append(RightReversed, Left, ReversedList),
    Left \= [],
    expand(Left, E),
    reverse(RightReversed, Right),
    encode(Right, ETail).

/* List is only expandable if it is a consecutive sequence */ 
expand([X|XT], [Count, X]) :- expand(XT, [SubCount, X]), succ(SubCount, Count).
expand([], [0, _]).

/* Signal morse function */ 
signal_morse([], []). 
signal_morse(List, Result) :- 
	encode(List, [[A, B] | _]),
	encode(Y, [[A, B]]), !,		
	signal(X, A, B), 
	append(X, Tail, Result),
	append(Y, Z, List),
	signal_morse(Z, Tail).

/* Signal message function */ 
signal_message(List, Message) :-
	signal_morse(List, Symbols), 
	convert(Symbols, LM), 
	remove_errors(LM, Message).  

/* Convert helper function for signal_message */
% convert symbols with accumulator
convert(S, R) :- convert(S, [], R). 

% base case 
convert([], [], []).

% Letter + carat 
convert([A|T], Accum, [X|BT]) :-
	reverse(Accum, Reversed), 
	morse(X, Reversed),
	is_carat([A|T], T),   
	convert(T, [], BT).

% Letter + pound sign 
convert([A|T], Accum, [X, A|BT]) :-
	reverse(Accum, Reversed), 
	morse(X, Reversed), 
	is_pound([A|T], T),  
	convert(T, [], BT).

% No match, keep going 
convert([A|AT], Accum, B) :-
	convert(AT, [A|Accum], B), !. 

% Final letter 
convert(A, Accum, [X|BT]) :- 
	reverse(Accum, Reversed), 
	morse(X, Reversed),  
	convert(A, [], BT).

/* Remove error helper for signal_message */ 
/* I assume that errors can be words */ 
/* Therefore [m,o,r,s,e,#,c,o,error,error,error,as] */
/* becomes [m,o,r,s,e,#,as]. */ 
remove_errors(M, R) :- remove_errors(M, [], R).

% base case
remove_errors([], [], []). 

% optional word followed by pound found 
remove_errors([A|T], Accum, R) :-
	is_pound([A|T], T),
	reverse([A|Accum], Reversed), 
	append(Reversed, RT, R), 
	remove_errors(T, [], RT), !.

% error seen, nonempty accumulator -> discard accumulator
remove_errors([A|T], Accum, R) :-
	is_error([A|T], T),
	Accum \= [], 
	remove_errors(T, [], R). 

% no words before error, or no match yet 
remove_errors([A|T], Accum, R) :-
	remove_errors(T, [A|Accum], R), !.

% Final word
remove_errors(A, Accum, R) :-
	reverse(Accum, Reversed), 
	append(Reversed, RT, R), 
	remove_errors(A, [], RT). 

% Helpers
is_carat([^|X], X).
is_pound([#|X], X). 
is_error([error|X], X). 


	



	