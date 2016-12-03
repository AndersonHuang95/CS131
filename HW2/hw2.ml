(* CS131 HW2 
 * convert_grammar translates grammars into HW2 form 
 * parse_prefix returns matcher for a grammar gram 
 *)

type ('nonterminal,'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(* Returns a grammar's alternative list for a nonterminal *)
let rec prod_func rules nt = match rules with 
	| (lhs, rhs)::t -> if (lhs = nt) then rhs::(prod_func t nt) else (prod_func t nt)
	| [] -> []

(* Transforms (start_expr, rules) -> (start_expr, prod_func) *)
let convert_grammar gram1 = (fst gram1, prod_func(snd gram1))


(* Mutually recursive functions *)

(* Test a single rule; There are several cases to consider 
 * 1) If we exhaust a rhs (of a rule), we have matched a fragment (suffix may or may not be empty) 
 * 2) If fragment is empty & rhs is still unfinished, match is not right 
 * 3) If both rhs and fragment lists are still nonempty, then depending
 * on whether the rhs is nt/t, we go back to the alternative list, or continue with the current rhs
 *) 

let rec scan_rule prod_func rhs acceptor derivation fragment = match rhs with 
	| [] -> acceptor derivation fragment
	| h::t -> match fragment with 
		| [] -> None
		| f_head::f_tail -> match rhs with 
			| (N nonterminal)::nt_tail -> scan_alternative_list nonterminal prod_func (prod_func nonterminal) (scan_rule prod_func nt_tail acceptor) derivation fragment
			| (T terminal)::t_tail -> if (f_head = terminal) then scan_rule prod_func t_tail acceptor derivation f_tail else None

(* Go through entire alternative list 
 * If alternative list is empty, we have exhausted all valid possiblities 
 * Otherwise, attempt to match individual rhs's of rules to frag
 * We know the current rhs is a dead end if scan_rule returns None (see above)
 *) 
and scan_alternative_list start_symbol prod_func alternative_list acceptor derivation fragment = match alternative_list with 
	| [] -> None
	| h::t -> match scan_rule prod_func h acceptor (derivation@[start_symbol, h]) fragment with 
		| None -> scan_alternative_list start_symbol prod_func t acceptor derivation fragment
		| result -> result 

let parse_prefix gram accept frag = match gram with 
	| (start_symbol, prod_func) -> scan_alternative_list start_symbol prod_func (prod_func start_symbol) accept [] frag
