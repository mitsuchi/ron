:- consult('ron.pl').

test(Code, Query) :-
    code_rules(Code, _),
    query_string(Query).

tests :-
    code_rules("op 50 : _ -> _ ;", _),
    code_rules("op 50 : _ => _ ;", _),
    code_rules("op 50 : if _ then _ else _ ;", _),
    code_rules("op 50 : _ |- _ : _ ;", _),
    code_rules("op 50 : _ |- _ => _ ;", _),
    code_rules("op 50 : _ + _ = _ ;", _),
    code_rules("op 50 : succ _ ;", _),
    code_rules("op 50 : _ plus _ is _ ;", _),
    test("1 -> 2;", "1 -> 2"),
    test("1 -> 2; 2 -> 3;", "2 -> 3"),
    test("1 -> 2; 2 -> 3; x => y {x -> y; } x => y {x -> z; z => y; }", "1 => 2"),
    test("c |- true : bool;", "c |- true : bool"),
    test("0 + n = n; succ m + n = succ p {m + n = p; }", "succ 0 + succ 0 = succ succ 0"),
    test("c |- 0 : int; c |- true : bool; c |- if e1 then e2 else e3 : t { c |- e1 : bool; c |- e2 : t; c |- e3 : t;}",
        "_ |- if true then 0 else 0 : int"),
    test("c |- 0 => 0; c |- 1 => 1; c |- true => true; c |- if e1 then e2 else e3 => v { c |- e1 => true; c |- e2 => v; }",
            "_ |- if true then 0 else 1 => 0"),
    test("", "1 -> (2)"),
    test("op 50 : _ + _ = _ ;0 plus n is n; (succ m) plus n is (succ p) { m plus n is p; }", "(succ 0) plus (succ 0) is (succ succ 0)"),
    test_arrow1,
    test_arrow2,
    test_arrow3,
    test_arrow4,
    test_arrow5,
    test_type1,
    test_succ,
    test_type2,
    test_plus,
    test_eval_if,
    test_skip.

test_arrow1 :- code_mi("op 50 : _ -> _ ; 1 -> 2; main { 1 -> 2; }").
test_arrow2 :- code_mi("op 50 : _ -> _ ; 1 -> 2; main { 1 -> x; }").
test_arrow3 :- code_mi("op 50 : _ -> _ ; 1 -> 2; main { x -> 2; }").
test_arrow4 :- code_mi("op 50 : _ -> _ ; 1 -> 2; 2 -> 3; main { 2 -> 3; }").
test_arrow5 :- code_mi("op 50 : _ -> _ ; op 50 : _ => _ ; 1 -> 2; 2 -> 3; x => y {x -> y; } x => y {x -> z; z => y; } main { 1 => 2; }").
test_type1 :- code_mi("op 50 : _ |- _ : _ ; _ |- true : bool; main { c |- true : x; }").
test_succ :- code_mi("op 50 : _ + _ = _ ; op 50 : succ _ ; 0 + n = n; succ m + n = succ p {m + n = p; } main {succ 0 + succ 0 = x; }").
test_type2 :- code_mi("op 50 : if _ then _ else _ ; op 50 : _ |- _ : _ ; _ |- 0 : int; _ |- true : bool; c |- if e1 then e2 else e3 : t { c |- e1 : bool; c |- e2 : t; c |- e3 : t; } main { _ |- if true then 0 else 0 : t ; }").
test_plus :- code_mi("op 50 : _ + _ = _ ; op 50 : succ _ ; op 50 : _ plus _ is _ ; 0 plus n is n; (succ m) plus n is (succ p) { m plus n is p; } main { (succ 0) plus (succ 0) is x ; }").
test_eval_if :- code_mi("op 50 : _ |- _ => _ ; op 50 : if _ then _ else _ ; op 50 : _ |- _ : _ ; c |- 0 => 0; c |- 1 => 1; c |- true => true; c |- if e1 then e2 else e3 => v { c |- e1 => true; c |- e2 => v; } main { _ |- if true then 0 else 1 => v ; }").

test_skip :- code_mi("; op 50 : _ -> _ ; 1 -> 2; main { 1 -> 2; } ; ;").

test_main :-
    code_mi("op 50 : _ -> _ ;"),
    code_mi("1 -> 2; 2 -> 3; main { x -> 3; }").

test_lf :-
    code_mi("op 50 : _ -> _
        1 -> 2
        2 -> 3
        main {
            x -> 3
        }
    ").

test_edge :-
    code_mi("op 50 : _ -> _ ;"),
    code_mi("op 50 : _ => _ ;"),
    code_mi("1 -> 2; 2 -> 3; 3 -> 4; x => y {x -> y; } x => y {x -> z; z => y; }").

test_app :-
    code_mi("op 50 : _ + _ ;"),
    code_mi("op 100 : _ _ ;"),
    code_pred("1 + 2 + 3", W), writeln(W).

test_ski2 :- code_mi("op 50 : _ -> _ ; op 50 : _ => _ ; op 100 : _ _ ;").

test_ski :-
    code_mi("op 50 : _ -> _ ;"),
    code_mi("op 50 : _ => _ ;"),
    code_mi("op 100 : _ _ ;"),
    code_mi("I x -> x ;"),
    code_mi("K x y -> x ;"),
    code_mi("S x y z -> x z (y z) ;"),
    code_mi("x y -> z y { x -> z; }"),    
    code_mi("x y -> x z { y -> z; }"),
    code_mi("x => y { x -> y; }"),
    code_mi("x => y { x -> z; z => y; }"),
    query_string("S K S K => x").

test_let :-
    code_mi("op 50 : _ + _ ;"),
    code_mi("op 40 : let _ = _ in _ ;").
test_calc :-
    code_mi("op 50 : _ ++ _ ;"),
    code_mi("op 60 : _ ** _ ;"),
    code_pred_canonical("(1 ++ 2) ** 3", W), str(W, U), writeln(U),
    code_pred_canonical("1 ** (2 ++ 3)", W2), str(W2, U2), writeln(U2).

:- tests.
:- halt.