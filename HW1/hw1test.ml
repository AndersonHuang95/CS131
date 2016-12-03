(* Test Cases for HW 1 *)

(* All test cases should return true except for the second to last case *)

let subset_test0 = subset [] [2;4;6]
let subset_test1 = subset [2;1;1] [1;2;3]
let subset_test2 = not (subset [1;1] [2;2])

let equal_sets_test0 = not (equal_sets [1;2;2] [1;2;3])
let equal_sets_test1 = equal_sets [1;5] [1;5]

let set_union_test0 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]

let set_intersection_test0 = equal_sets (set_intersection [1;3;5] [2;4;6]) []

let set_diff_test0 = equal_sets (set_diff [1;2;3;4;5] [1;3;5]) [2;4]

let computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 5) 10 = 0
let computed_fixed_point_test1 = computed_fixed_point (=) (fun z -> z *. 2.) 20. = infinity 

let computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x - 2) 0 2 = 2
let computed_periodic_point_test1 = computed_periodic_point (=) (fun z -> z *. z -. 1.) 2 0.5 = -1.

let while_away_test0 = equal_sets (while_away ((+) 1) ((>) 10) 0) [0;1;2;3;4;5;6;7;8;9]
let while_away_test1 = equal_sets (while_away ((+) 5) ((>) 25) 0) [0;5;10;15;20]

let rle_decode_test0 = equal_sets (rle_decode [1, "h"; 1, "e"; 2, "l"; 5, "o"]) ["h"; "e"; "l"; "l"; "o"; "o"; "o"; "o"; "o"]
let rle_decode_test1 = equal_sets (rle_decode [1, 1; 2, 2; 3, 3; 4, 4]) [1;2;2;3;3;3;4;4;4;4]

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Lvalue, [T"$"; N Lvalue];
    Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Incrop];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

