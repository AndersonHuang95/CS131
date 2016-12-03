(* Eggert's test grammar and sample test cases *)

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
   [[N Num];
    [N Lvalue];
    [N Incrop; N Lvalue];
    [N Lvalue; N Incrop];
    [T"("; N Expr; T")"]]
     | Lvalue ->
   [[T"$"; N Expr]]
     | Incrop ->
   [[T"++"];
    [T"--"]]
     | Binop ->
   [[T"+"];
    [T"-"]]
     | Num ->
   [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
    [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  ((parse_prefix awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  ((parse_prefix awkish_grammar accept_all ["9"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], []))

let test2 =
  ((parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
   (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "1"])],
  ["+"]))

let test3 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

(* This one might take a bit longer.... *)
let test4 =
 ((parse_prefix awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some
     ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
       (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term]); (Term, [N Num]); (Num, [T "8"]); (Binop, [T "-"]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "--"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Num]); (Num, [T "9"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "9"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Incrop, [T "--"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])],
      []))

let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)

let test5 =
  ((parse_prefix awkish_grammar accept_only_non_lvalues
      ["3"; "-"; "4"; "+"; "$"; "5"; "-"; "6"])
   = Some
      ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
  (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
       ["+"; "$"; "5"; "-"; "6"]))

(* A very small subset of English grammar and two custom test cases *)

type english_subset_nonterminals = 
  | Sentence | Subject | Predicate | PrepPhrase | Noun | Verb | Article | Conjunction | Preposition | Adjective | Adverb 


let english_subset_rules = 
  [Sentence, [N Subject; N Predicate];
   Sentence, [N Sentence; N Conjunction; N Sentence];
   Subject, [N Article; N Adverb; N Adjective; N Noun];
   Subject, [N Article; N Adjective; N Noun];
   Subject, [N Article; N Noun]; 
   Subject, [N Noun]; 
   Subject, [N Noun; N Conjunction; N Noun];
   Predicate, [N Verb; N PrepPhrase];
   Predicate, [N Adverb; N Verb; N PrepPhrase]; 
   Predicate, [N Verb]; 
   Predicate, [N Verb; N Noun];
   Predicate, [N Verb; N Noun; N PrepPhrase]; 
   Predicate, [N Adverb; N Noun]; 
   PrepPhrase, [N Preposition; N Article; N Noun]; 
   PrepPhrase, [N Preposition; N Noun]; 
   PrepPhrase, [N Preposition; N Article; N Adjective; N Noun]; 
   Noun, [T "Jack"]; 
   Noun, [T "Jill"]; 
   Noun, [T "He"]; 
   Noun, [T "She"]; 
   Noun, [T "They"]; 
   Noun, [T "city"]; 
   Noun, [T "prairie"]; 
   Noun, [T "library"]; 
   Noun, [T "kitchen"];
   Verb, [T "walked"]; 
   Verb, [T "ate"]; 
   Verb, [T "played"]; 
   Verb, [T "slept"]; 
   Verb, [T "explored"]; 
   Article, [T "a"]; 
   Article, [T "an"]; 
   Article, [T "the"]; 
   Conjunction, [T "for"]; 
   Conjunction, [T "and"];
   Conjunction, [T "nor"]; 
   Conjunction, [T "but"]; 
   Conjunction, [T "or"];  
   Conjunction, [T "yet"]; 
   Conjunction, [T "so"]; 
   Preposition, [T "after"];
   Preposition, [T "before"];
   Preposition, [T "on"]; 
   Preposition, [T "in"];  
   Preposition, [T "through"]; 
   Preposition, [T "across"]; 
   Preposition, [T "until"];
   Preposition, [T "along"]; 
   Adjective, [T "happy"]; 
   Adjective, [T "sad"]; 
   Adjective, [T "beautiful"]; 
   Adjective, [T "expansive"];
   Adjective, [T "delicious"]; 
   Adjective, [T "cozy"];
   Adverb, [T "enthusiastically"]; 
   Adverb, [T "seldom"]; 
   Adverb, [T "suddenly"]]

let english_subset_grammar = Sentence, english_subset_rules

let english_subset_hw2_grammar = convert_grammar english_subset_grammar

let test_1 =
  ((parse_prefix english_subset_hw2_grammar accept_all ["Jack"; "and"; "Jill"; "enthusiastically"; "walked"; "across"; "the"; "beautiful"; "city"])
   = Some
   ([(Sentence, [N Subject; N Predicate]);
     (Subject, [N Noun; N Conjunction; N Noun]); 
     (Noun, [T "Jack"]);
     (Conjunction, [T "and"]); 
     (Noun, [T "Jill"]);
     (Predicate, [N Adverb; N Verb; N PrepPhrase]);
     (Adverb, [T "enthusiastically"]); 
     (Verb, [T "walked"]);
     (PrepPhrase, [N Preposition; N Article; N Adjective; N Noun]);
     (Preposition, [T "across"]); 
     (Article, [T "the"]);
     (Adjective, [T "beautiful"]); 
     (Noun, [T "city"])],
    []))

let test_2 = 
  ((parse_prefix english_subset_hw2_grammar accept_all ["He"; "ate"; "in"; "the"; "cozy"; "kitchen"]) 
  = Some
  ([(Sentence, [N Subject; N Predicate]);
  (Subject, [N Noun]);
  (Noun, [T "He"]);
  (Predicate, [N Verb; N PrepPhrase]);
  (Verb, [T "ate"]);
  (PrepPhrase, [N Preposition; N Article; N Adjective; N Noun]);
  (Preposition, [T "in"]); 
  (Article, [T "the"]);
  (Adjective, [T "cozy"]); 
  (Noun, [T "kitchen"])],
  []))






