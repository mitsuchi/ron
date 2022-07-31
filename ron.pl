:- set_prolog_flag(double_quotes, chars).
:- dynamic ops/4.
:- dynamic '_main'/0.

% tokenize
tokens(Ts) --> " ", tokens(Ts).
tokens([T|Ts]) --> tok(T), !, tokens(Ts).
tokens([]) --> "".

tok(N) --> num(N).
tok(;) --> ['\n'].
tok(;) --> ";".
tok(Atom) --> puncts(Cs), {atom_chars(Atom, Cs)}.
tok(Atom) --> word(Cs), {length(Cs, N), N > 1, atom_chars(Atom, Cs)}.
tok(Atom) --> [C], {code_type(C, upper), atom_chars(Atom, [C])}.
tok(Alnum) --> alnum(Alnum).
tok(Alpha) --> alpha(Alpha).
num(N) --> digits(Cs), { number_chars(N, Cs) }.
word([C|Cs]) --> [C], { code_type(C, lower) }, word(Cs).
word([C]) --> [C], { code_type(C, lower) }.
alpha(T) --> [C], { code_type(C, lower), T = '$VAR'(C) }.
alnum(T) --> [C], [N],
    { code_type(C, lower), code_type(N, digit), atom_concat(C, N, CN), T = '$VAR'(CN) }.
puncts([C|Cs]) --> [C], { code_type(C, punct) }, puncts(Cs).
puncts([C]) -->  [C], { code_type(C, punct) }.
digits([C|Cs]) --> digit(C), digits(Cs).
digits([C])    --> digit(C).
digit(C)   --> [C], { code_type(C, digit) }.

skip(W) --> "" | (W, skip(W)).

% mixfix parser
% https://zenn.dev/pandaman64/books/pratt-parsing
% https://qiita.com/h_sakurai/items/40abccdaad4728e0602e

e(P,O)-->
    [U], 
    {not(U = ';'), not(U = '{'), not(U = '}') },
    t(P,U,O).
t(P,T,O)-->
    {ops(A, _, leading, [L|M])},
    {L=T},     
    f(M,V),             
    {call(A,V,W)},      
    t(P,W,O).           
t(P,T,O)-->   
    [U],      
    {not(U = ';'), not(U = '{'), not(U = '}') },
    {ops(A, _, following, [L,L2|M])},
    {L2=U, P<L},        
    f(M,V),             
    {call(A,[T|V],W)},   
    t(P,W,O).            
t(P,T,O)-->
    {ops(A, _, following, [L,L2|M])},
    {number(L2), P<L},
    g([L2|M],V),      
    {call(A,[T|V],W)},
    t(P,W,O)          
    ; {T=O}.          
f([P|N],R)-->         
    (
        {number(P),!},
        e(P,T),       
        {R=[T|I]}     
        ;[P],         
        {R=I}),       
    f(N,I).           
f([],[])-->!.         

g([P|N],R)-->         
    e1(P,T),         
    {R=[T|I]},       
    g(N,I).            
g([],[])-->!.          

e1(P,O)-->    
    [U],     
    {not(U = ';'), not(U = '{'), not(U = '}') },
    {number(U) ; variable(U); all_alpha(U); U = '('; U = '{'},
    t(P,U,O).

a(P,T,R) :- R=..[P|T].

variable(U) :- U = '$VAR'(_).

all_alpha(U) :-
    not(U = '$VAR'(_)),
    atom_chars(U, Cs), all_alpha_chars(Cs).

all_alpha_chars([C]) :- code_type(C, lower).
all_alpha_chars([C]) :- code_type(C, upper).
all_alpha_chars([C|Cs]) :- code_type(C, alpha), all_alpha_chars(Cs).

all_punct(U) :-
    not(U = '$VAR'(_)),
    atom_chars(U, Cs), all_punct_chars(Cs).

all_punct_chars([C]) :- code_type(C, punct).
all_punct_chars([C|Cs]) :- code_type(C, punct), all_punct_chars(Cs).

% parse
% rules ::= rule | rule rules
% rule ::= pred ';' | pred '{' body '}'
% body ::= pred ';' | pred ';' body
rules([R | Rs]) --> skip(";"), rul(R), {add_rule(R)}, rules(Rs).
rules([]) --> skip(";").

rul(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".
rul(P :- true) --> pred(P), ";".
rul(P :- B) --> pred(P), "{", skip(";"), body(B), "}".

notation([R]) --> [R], {not(R = ';')}.
notation([R|Rs]) --> [R], {not(R = ';')}, notation(Rs).

pred(O) --> e(0, O).

body(P) --> pred(P), skip(";").
body((P,Bs)) --> pred(P), ";", body(Bs).

% make rules
add_rules([R]) :- add_rule(R).
add_rules([R | Rs]) :- add_rule(R), add_rules(Rs).

add_rule(op(Prec, ['_' | Ns])) :-
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct(Ns, Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    %write('rule1: '), writeln(Term),
    assert(Term).
add_rule(op(Prec, [N | Ns])) :-
    N \= '_',
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct([N|Ns], Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    %write('rule2: '), writeln(Term),
    assert(Term).
add_rule(Head :- Body) :-
    canonical(Head, HeadC),
    canonical(Body, BodyC),
    (Head = main -> query(BodyC);
    varnumbers_names(HeadC :- BodyC, Term, _),
    %write('rule3: '), writeln(Term),
    assert(Term), !).

% () はあらかじめ定義しておく
ops(a('()'), 0, leading, ['(',0,')']).

% Prolog Term としての標準記法に変換
canonical((B, Bs), (P, Ps)) :-
    canonical(B, P),
    canonical(Bs, Ps).
canonical(Term, Canonical) :-
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, canonical(Arg, Canonical)
    % $VAR はそのまま
    ; Functor = '$VAR' -> Canonical = Term
    % それ以外は Functor の先頭に _ をつける
    ; atomic_concat('_', Functor, FunctorC),
                        canonicalList(Args, ArgsC),
                        Canonical =.. [FunctorC | ArgsC]).  
canonical(T, T).

canonicalList([], []).
canonicalList([A|As], [C|Cs]) :-
    canonical(A,C),
    canonicalList(As, Cs).

replaceUnderScore([A|As], R, [R_|Bs]) :-
    replaceUnderScore(A, R, R_),
    replaceUnderScore(As, R, Bs).
replaceUnderScore([A], R, [R_]) :-
    replaceUnderScore(A, R, R_).
replaceUnderScore([], _, []).
replaceUnderScore('_', R, R).
replaceUnderScore(A, _, A) :- A \= '_'.

pickPunct(['_'|Ns], Punct) :-
    pickPunct(Ns, Punct).
pickPunct([N|Ns], Punct) :-
    N \= '_',
    pickPunct(Ns, Ps),
    atom_concat(N, Ps, Punct).
pickPunct([], '').

% 問い合わせ
query(C) :-
    varnumbers_names(C, T, P), !,
    call(T),
    (P \= [] -> unparseAnswers(P) ; true).

query_string(S) :-
    code_pred_canonical(S, C),
    !,
    query(C).

unparseAnswers([X = A|Ps]) :-
    str(A, F), atomics_to_string([X, =, F], ' ', UA), writeln(UA),
    unparseAnswers(Ps).
unparseAnswers([]).

str(A, A) :- atom(A); number(A).
str(E, Str) :-
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    %ops(Op, _, _, As),
    atom_concat('_', Op1, Op), 
    ops(a(Op1), _, _, As),
    str_each(Op1, As, Terms, Strs, false),
    atomic_list_concat(Strs, ' ', Str).

str_each(_, [], _, [], _).
str_each(Op, [A|As], [Term|Ts], [Str1|Rest], Paren) :-
    number(A),
    str(Term, Op, Str1, Paren),
    str_each(Op, As, Ts, Rest, true).
str_each(Op, [A|As], Ts, [A|Rest], Paren) :-
    not(number(A)),
    str_each(Op, As, Ts, Rest, Paren).

str(A, _, A, _) :- atom(A); number(A).
str(E, Op1, Str, Paren) :-
    functor(E, _, _, compound),
    E =.. [Op | _],
    atom_concat('_', Op2, Op), 
    ops(a(Op2), P2, _, _),
    ops(a(Op1), P1, _, _),
    ((P1 > P2 ; Paren, Op1 = Op2) -> str(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
            ; str(E, Str)).

code_tokens(Code, Tokens) :-
    phrase(tokens(Tokens), Code).

code_rules(Code, Rules) :-
    code_tokens(Code, Tokens),
    phrase(rules(Rules), Tokens).

code_pred(Code, Pred) :-
    code_tokens(Code, Tokens),
    phrase(pred(Pred), Tokens).

code_pred_canonical(Code, C) :-
    code_pred(Code, Pred),
    canonical(Pred, C).
    
code_mi(Code) :-
    %code_rules(Code, Rules),
    code_rules(Code, _).
    %writeln(Rules).
    %maplist(writeln, Rules).
    %add_rules(Rules).

test(Code, Query) :-
    code_mi(Code),
    query_string(Query).

tests :-
    code_mi("op 50 : _ -> _ ;"),
    code_mi("op 50 : _ => _ ;"),
    code_mi("op 50 : if _ then _ else _ ;"),
    code_mi("op 50 : _ |- _ : _ ;"),
    code_mi("op 50 : _ |- _ => _ ;"),
    code_mi("op 50 : _ + _ = _ ;"),
    code_mi("op 50 : succ _ ;"),
    code_mi("op 50 : _ plus _ is _ ;"),
    test("1 -> 2;", "1 -> 2"),
    test("1 -> 2; 2 -> 3;", "2 -> 3"),
    test("1 -> 2; 2 -> 3; x => y {x -> y; } x => y {x -> z; z => y; }", "1 => 2"),
    test("_ |- true : bool;", "c |- true : x"),
    test("0 + n = n; succ m + n = succ p {m + n = p; }", "succ 0 + succ 0 = x"),
    test("_ |- 0 : int; c |- if e1 then e2 else e3 : t { c |- e1 : bool; c |- e2 : t; c |- e3 : t; }",
        "_ |- if true then 0 else 0 : t"),
    test("c |- 0 => 0; c |- 1 => 1; c |- true => true; c |- if e1 then e2 else e3 => v { c |- e1 => true; c |- e2 => v; }", "_ |- if true then 0 else 1 => v"),
    test("1 -> 2;", "1 => (3)"),
    test("1 -> 2;", "(1 => 3)"),
    test("op 50 : _ + _ = _ ;0 plus n is n; (succ m) plus n is (succ p) { m plus n is p; }", "(succ 0) plus (succ 0) is x"),
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

test_eval_if_lf :- code_mi("
    op 50 : _ |- _ => _
    op 50 : if _ then _ else _
    
    c |- 0 => 0
    c |- 1 => 1
    c |- true => true
    c |- if e1 then e2 else e3 => v {
        c |- e1 => true
        c |- e2 => v
    }
    
    main {
        _ |- if true then 0 else 1 => v
    }
    ").

test_type2_lf :- code_mi("
    op 50 : if _ then _ else _
    op 50 : _ |- _ : _ 

    _ |- 0 : int
    _ |- true : bool
    c |- if e1 then e2 else e3 : t {
        c |- e1 : bool
        c |- e2 : t
        c |- e3 : t
    }

    main {
        _ |- if true then 0 else 0 : t
    }
    ").

test_arrow1_lf :- code_mi("
    op 50 : _ -> _
    1 -> 2

    main {
        1 -> 2
    }
    ").

test_arrow2_lf :- code_mi("
    op 50 : _ -> _
    op 50 : _ => _

    1 -> 2
    2 -> 3

    x => y {
        x -> y
    }
    x => y {
        x -> z
        z => y
    }

    main {
        1 => x
    }
    ").

test_arrow3_lf :- code_mi("
    op 50 : _ -> _
    op 50 : _ => _

    1 -> 2; 2 -> 3;
    x => y { x -> y }
    x => y { x -> z; z => y }

    main {
        1 => x
    }
    ").

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

test_ski_lf :- code_mi("
    op 50 : _ -> _
    op 50 : _ => _
    op 100 : _ _

    I x     -> x
    K x y   -> x
    S x y z -> x z (y z)

    x y -> z y { x -> z }
    x y -> x z { y -> z }
    x => y     { x -> y }
    x => y     { x -> z; z => y }

    main {
        S K S K => x
    }
    ").

test_ski2 :- code_mi("op 50 : _ -> _ ; op 50 : _ => _ ; op 100 : _ _ ;").

test_succ_lf :- code_mi("
    op 50 : _ + _ = _
    op 50 : succ _

    0 + n = n
    succ m + n = succ p {
        m + n = p
    }

    main {
        succ 0 + succ 0 = x
    }
    ").

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

test_plus_lf :- code_mi("
    op 50 : S _
    op 50 : _ plus _ is _
    
    Z plus n is n
    (S n1) plus n2 is (S n) {
        n1 plus n2 is n
    }

    main {
        (S Z) plus (S Z) is x
    }
    ").