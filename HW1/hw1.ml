(* Homework 1 
CS 131 - Eggert *)

(* 1 *)

let rec contains e l = match l with
	| [] -> false
	| h::t -> if e = h then true else contains e t

let rec subset a b = match a with 
	| [] -> true
	| h::t -> if contains h b then subset t b else false 

(* 2 *)

let equal_sets a b = subset a b && subset b a 

(* 3 *)

let set_union a b = List.append a b 

(* 4 *)

let rec set_intersection a b = match a with 
	| [] -> []
	| h::t -> if contains h b then h::(set_intersection t b) else set_intersection t b 

(* 5 *)

let rec set_diff a b = match a with 
	| [] -> []
	| h::t -> if not(contains h b) then h::(set_diff t b) else set_diff t b 

(* 6 *)

let rec computed_fixed_point eq f x = if eq x (f x) then x else computed_fixed_point eq f (f x)

(* 7 *)

let rec periodic_point eq f p x = match (p > 0) with 
	| true -> periodic_point (eq) (f) (p-1) (f x)
	| false -> x 

let rec computed_periodic_point eq f p x = if eq x (periodic_point eq f p x) then x else computed_periodic_point eq f p (f x)

(* 8 *)

let rec while_away s p x = match p x with 
	| true -> x::(while_away s p (s x))
	| false -> []

(* alternatively 
let rec while_away s p x = if (p x) then x::(while_away s p (s x )) else []
*)

(* 9 *)

let rec rle_helper a b = if (a > 0) then b::(rle_helper (a-1) (b)) else []

let rec rle_decode lp = match lp with 
	| [] -> []
	| h::t -> (rle_helper (fst h) (snd h))@(rle_decode t) 

(* 10 *)

type ('nonterminal,'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(* Idea: Build up a list of nonterminal symbols that can be reduced to terminal symbols 
 * and for each rule the grammar (which consists of a start symbol and a set or rules) contains,
 * check if it can be reduced. If it can, keep the rule. Else discard it. *)
 
(* Objects of type T are automatically terminable; only examine objects of type N *) 
let rec is_terminable rhs terminal_values = match rhs with 
	| [] -> true 
	| h::t -> match h with 
		| N (a) -> if not(contains a terminal_values) then false else is_terminable t terminal_values
		| T (a) -> is_terminable t terminal_values 

(* Go through rules and see if any new rhs of rules are terminable (this only does 1 iteration) 
 * There are 3 cases
 * 1) symbol is already in list
 * 2) symbol not in list, but can be reduced to terminal symbols
 * 3) symbol not in list, but cannot (currently, or ever) be reduced *) 

let rec build_terminable_list rules terminal_values = match rules with 
	| [] -> terminal_values
	| h::t -> if (contains (fst h) terminal_values) then build_terminable_list t terminal_values 
		else if (is_terminable (snd h) (terminal_values)) then build_terminable_list t (terminal_values@[fst h])
		else build_terminable_list t terminal_values 

(* Compute list again and again until no more changes occur *)
let construct_terminable_list rules = computed_fixed_point (=) (build_terminable_list rules) []

(* let rec construct_terminable_list current prev rules = if (equal_sets (build_terminable_list rules current) prev) 
 * then current else construct_terminable_list (build_terminable_list rules current) current rules *)

(* Assuming a complete terminable list has been generated, compare with rules and discard all rules with nonterminable symbols *)
let rec discard_nonterminables rules terminal_values = match rules with 
	| [] -> []
	| h::t -> if (is_terminable (snd h) terminal_values) then [h]@(discard_nonterminables t terminal_values) else discard_nonterminables t terminal_values

(* The real work 
 * From right to left construct a list of terminable symbols (reach a fixed point at some time), 
 * Compare each rhs of original rules and one by one determine if they are terminable
 * Throw out those that aren't *) 
let filtered_rules rules = discard_nonterminables (rules) (construct_terminable_list rules)

let filter_blind_alleys g = match g with
	| (start, rules) -> (start, (filtered_rules rules))