:- set_prolog_flag(double_quotes, chars).
:- dynamic ops/4.
:- dynamic '_main'/0.

% tokenize
tokens(Ts) --> " ", tokens(Ts).
tokens([T|Ts]) --> tok(T), !, tokens(Ts).
tokens([]) --> "".

tok(N) --> num(N).
tok(;) --> ['\n'].
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

% mixfix library
e(P,O)-->     % 前方演算子の後方束縛力が P のとき、トークン列をパーズして構築されるAST を O とする
    [U],      % 先頭のトークンが U のとき
    t(P,U,O). % 前方演算子の後方束縛力を P、ここまで構築済みのASTを U として、AST を構築して O とする
t(P,T,O)-->   % 前方演算子の後方束縛力が P、ここまで構築済みのASTが T のとき、AST を構築して O とする
    %{[L|M]^A},  % 演算子表から1行を探して、演算子の先頭を L, 残りをM、演算子名を A とする
    {ops(A, _, leading, [L|M])},  % 演算子表から1行を探して、演算子の先頭を L, 残りをM、演算子名を A とする
    {L=T},     % L と T が unify できるかを見る。
                        % できる場合は先頭のトークンが演算子の先頭部分と一致している。この場合は以下へ。
    f(M,V),             % 演算子の残りを Mとするとき、パーズして、演算子の引数のリストを V に入れる
    {call(A,V,W)},      % a(演算子名,引数リスト,W) を呼ぶ。W には 演算子名(引数1, 引数2, ...) が入る。
    t(P,W,O).           % 前方演算子の後方束縛力を P、ここまで構築済みのASTを Wとして、AST を構築して O とする
t(P,T,O)-->   % 前方演算子の後方束縛力が P、ここまで構築済みのASTが T のとき、AST を構築して O とする
    [U],      % 次のトークンを読んで U とする
    %{[L,L2|M] ** A},    % 中置演算子表から1行を探して、演算子の先頭を L, 次を L2, 残りをM、演算子名を A とする
    {ops(A, _, following, [L,L2|M])},    % 中置演算子表から1行を探して、演算子の先頭を L, 次を L2, 残りをM、演算子名を A とする
    {L2=U, P<L},        % L2 と U が unify できるかを見る。
                        % できる場合は先頭のトークンが演算子の先頭部分と一致している。
                        % その場合、Lは前方束縛力。L のほうが大きい場合は同様に以下へ。
    f(M,V),             % 演算子の残りを Mとするとき、パーズして、演算子の引数のリストを V に入れる
    {call(A,[T|V],W)},   % a(演算子名,引数リスト,W) を呼ぶ。W には 演算子名(引数1, 引数2, ...) が入る。
    t(P,W,O).            % 前方演算子の後方束縛力を P、ここまで構築済みのASTを Wとして、AST を構築して O とする
t(P,T,O)-->   % 前方演算子の後方束縛力が P、ここまで構築済みのASTが T のとき、AST を構築して O とする
    %{[L,L2|M] ** A},    % 中置演算子表から1行を探して、演算子の先頭を L, 次を L2, 残りをM、演算子名を A とする
    {ops(A, _, following, [L,L2|M])},    % 中置演算子表から1行を探して、演算子の先頭を L, 次を L2, 残りをM、演算子名を A とする
    {number(L2), P<L},  % L2 が数字ならLは前方束縛力。L のほうが大きい場合は同様に以下へ。
    g([L2|M],V),        % 演算子の残りを [L2|M] とするとき、パーズして、演算子の引数のリストを V に入れる
    {call(A,[T|V],W)},   % a(演算子名,引数リスト,W) を呼ぶ。W には 演算子名(引数1, 引数2, ...) が入る。
    t(P,W,O)            % 前方演算子の後方束縛力を P、ここまで構築済みのASTを Wとして、AST を構築して O とする
    ; {T=O}.            % ここまでの条件を満たさないとき、ここまで構築済みの T を結果として返す         
f([P|N],R)-->           % 演算子の先頭が P、残りが Nのとき、パーズして演算子の引数のリストをRに入れる。
    (
        {number(P),!},  % 演算子の先頭が数値なら次へ、ただしこの条件では以後トラックバックしない
        e(P,T),         % 前方演算子の後方束縛力を P として、構築される AST を T とする
        {R=[T|I]}       % 構築される AST を先頭に、残りを I として結果を R とする
        ;[P],           % 演算子の先頭が数値でないなら、1トークン分読み進める
        {R=I}),         % 結果は残りとする
    f(N,I).             % 再帰的に、演算子の残りを Nとして処理し、引数リストを I に入れる。
f([],[])-->!.           % 演算子の残りが空の場合は、引数リストも空。

g([P|N],R)-->           % 演算子の先頭が P、残りが Nのとき、パーズして演算子の引数のリストをRに入れる。
    e1(P,T),         % 前方演算子の後方束縛力を P として、構築される AST を T とする
    {R=[T|I]},       % 構築される AST を先頭に、残りを I として結果を R とする
    g(N,I).             % 再帰的に、演算子の残りを Nとして処理し、引数リストを I に入れる。
g([],[])-->!.           % 演算子の残りが空の場合は、引数リストも空。

e1(P,O)-->     % 前方演算子の後方束縛力が P のとき、トークン列をパーズして構築されるAST を O とする
    [U],      % 先頭のトークンが U のとき
    %{all_punct(U) -> fail; true},
    %{(U = ';'; U = '->' ; U = '=>'; U = '+') -> fail; true},
    {number(U) ; variable(U); all_alpha(U); U = '('; U = '{'},
    %{write(ok), write(' '), writeln(U)},    
    t(P,U,O). % 前方演算子の後方束縛力を P、ここまで構築済みのASTを U として、AST を構築して O とする

a(P,T,R) :- R=..[P|T].

variable(U) :-
    U = '$VAR'(_).
    %(U = ';'; U = '->' ; U = '=>'; U = '+') -> fail; true.

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
rules([R]) --> rul(R).
rules([R | Rs]) --> rul(R), rules(Rs).

rul(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".
rul(P :- true) --> pred(P), ";".
rul(P :- B) --> pred(P), "{", body(B), "}".

notation([R]) --> [R].
notation([R|Rs]) --> [R], notation(Rs).

pred(O) --> e(0, O).

body(P) --> pred(P), ";".
body((P,Bs)) --> pred(P), ";", body(Bs).

% make rules
add_rules([R]) :- add_rule(R).
add_rules([R | Rs]) :- add_rule(R), add_rules(Rs).

add_rule(op(Prec, ['_' | Ns])) :-
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct(Ns, Punct),
    %Term = [Prec|Ns_] ** a(Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    %write('rule1: '), writeln(Term),
    assert(Term).
add_rule(op(Prec, [N | Ns])) :-
    N \= '_',
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct([N|Ns], Punct),
    %Term = [N | Ns_] ^ a(Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    %write('rule2: '), writeln(Term),
    assert(Term).
add_rule(Head :- Body) :-
    %write('rule3-0: '), writeln(Head), writeln(Body),
    canonical(Head, HeadC),
    canonical(Body, BodyC),
    %write('BodyC = '), writeln(BodyC),
    (Head = main -> query(BodyC);
    varnumbers_names(HeadC :- BodyC, Term, _),
    %write('rule3: '), writeln(Term),
    assert(Term)).

% () はあらかじめ定義しておく
%['(',0,')']^a('()').
ops(a('()'), 0, leading, ['(',0,')']).
%[0,;,0] ** a(';').
%[100,100] ** a('').

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

% AST を文字列に変換する
unparse(Term, ResultAtoms) :-
    functor(Term, _, _, compound),
    Term =.. [Op | Terms],
    unparse(Op, Terms, ResultAtoms)
    ; ResultAtoms = [Term].

% op(arg1, arg2, ...) の形の AST を 文字列に変換する
unparse(Op, Terms, ResultAtoms) :-
    atom_concat('_', Op1, Op), 
    %(As ** a(Op1) ; As ^ a(Op1)),      % 演算子表から探す
    ops(a(Op1), _, _, As),
    numToTerm(As, Terms, ResultAtoms).
  
% 
numToTerm([], _, []).

% [50, +, 51] で [a, b] なら [a, +, b] を返す
numToTerm([A| As], [Term|Ts], [AtomsRec|Rest]) :-
    number(A),
    functor(Term, _, _, compound),
    Term =.. [Op | Terms],
    unparse(Op, Terms, AtomsRec),
    numToTerm(As, Ts, Rest)
    ; (AtomsRec = Term, numToTerm(As, Ts, Rest)).
   
numToTerm([A|As], Ts, [A|Rest]) :-
    not(number(A)),
    numToTerm(As, Ts, Rest).

% 問い合わせ
query(C) :-
    %writeln(query(C)),
    varnumbers_names(C, T, P), !,
    call(T),
    %(P \= [] -> writeln(P), unparseAnswers(P) ; true).
    (P \= [] -> unparseAnswers(P) ; true).
    %(P \= [] -> writeln(P) ; true).

query_string(S) :-
    code_pred_canonical(S, C),
    !,
    query(C).

unparseAnswers([X = A|Ps]) :-
    %unparse(A, W), flatten(W, F), atomics_to_string([X, = | F], ' ', UA), writeln(UA),
    str(A, F), atomics_to_string([X, =, F], ' ', UA), writeln(UA),
    %unparse(A, W), flatten(W, F), writeln(X = F),
    %unparse(A, W), writeln(X = W),
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
    %writeln(Op1), writeln(P1),
    %writeln(Op2), writeln(P2),
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
    code_rules(Code, Rules),
    %writeln(Rules),
    add_rules(Rules).

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
    test("0 plus n is n; (succ m) plus n is (succ p) { m plus n is p; }", "(succ 0) plus (succ 0) is x").

test_main :-
    code_mi("op 50 : _ -> _ ;"),
    code_mi("1 -> 2; 2 -> 3; main { x -> 3; }").

test_lf :-
    append("op 50 : _ -> _", ['\n'], Code),
    code_mi(Code),
    code_mi("1 -> 2; 2 -> 3; main { x -> 3; }").

test_edge :-
    code_mi("op 50 : _ -> _ ;"),
    code_mi("op 50 : _ => _ ;"),
    code_mi("1 -> 2; 2 -> 3; 3 -> 4; x => y {x -> y; } x => y {x -> z; z => y; }").

test_app :-
    code_mi("op 50 : _ + _ ;"),
    code_mi("op 100 : _ _ ;"),
    code_pred("1 + 2 + 3", W), writeln(W).

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
    code_mi("x => y { x -> z; z => y; }"), !, 
    %query_string("ss kk ii (kk ii ss) => x").
    %code_pred_canonical("K K S K", W), str(W, U), writeln(U).
    query_string("S K S K => x").
    %query_string("S (K (S I) ) kk aa bb => x").
test_let :-
    code_mi("op 50 : _ + _ ;"),
    code_mi("op 40 : let _ = _ in _ ;").
test_calc :-
    code_mi("op 50 : _ ++ _ ;"),
    code_mi("op 60 : _ ** _ ;"),
    %code_pred("1 ++ 2 ** 3", W), unparse(W, U), writeln(U).
    %code_pred("1 ++ 2 ** 3", W), str(W, U), writeln(U).
    code_pred_canonical("(1 ++ 2) ** 3", W), str(W, U), writeln(U),
    code_pred_canonical("1 ** (2 ++ 3)", W2), str(W2, U2), writeln(U2).

