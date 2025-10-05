:- set_prolog_flag(double_quotes, chars).
:- dynamic ops/4.
:- dynamic '_main'/0.

:- initialization(run).

run :- 
    current_prolog_flag(argv, Argv),
    nth1(1, Argv, FilePath) ->
        (catch(
            file_eval(FilePath),
            Exception,
            (write('error: '), write(Exception), nl, fail)
        ), halt) ; true.

file_eval(FilePath) :-
    file_chars(FilePath, Chars),
    chars_eval(Chars).

file_chars(FilePath, Chars) :-
    read_file_to_string(FilePath, String, []),
    string_chars(String, Chars).

chars_eval(Chars) :-
    chars_ops_rules(Chars, _, Rules), !. % 第2引数は Ops
    %assert_rules(Rules)
    %query(main).

chars_ops_rules(Chars, Ops, Rules) :-
    chars_tokens(Chars, Tokens),
    writeln('Tokens = '), writeln(Tokens),
    tokens_ops(Ops, Tokens, RestTokens),
    writeln('Ops = '), writeln(Ops),
    writeln('RestTokens = '), writeln(RestTokens),
    assert_ops(Ops).
    %tokens_rules(Tokens, Rules),
    %writeln('Rules = '), writeln(Rules).
    %writeln(Rules),
    %assert_rules(Rules).

chars_tokens(Chars, Tokens) :-
    phrase(tokens(Tokens), Chars).

tokens_ops([R|Rs]) --> skip(";"), rule_op(R), !, tokens_ops(Rs).
tokens_ops([]) --> skip(";").

rule_op(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".
rule_pred(P :- true) --> pred(P), ";".
rule_pred(P :- B) --> pred(P), "{", skip(";"), body(B), "}".

assert_ops([R|Rs]) :- assert_op(R), assert_ops(Rs).
assert_ops([]). 

assert_op(op(Prec, ['_' | Ns])) :-
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct(Ns, Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    assert(Term).
assert_op(op(Prec, [N | Ns])) :-
    N \= '_',
    replaceUnderScore(Ns, Prec, Ns_),
    pickPunct([N|Ns], Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    assert(Term).
assert_op(_ :- _).

tokens_rules(Tokens, Rules) :-
    phrase(rules(Rules), Tokens).

assert_rules([R | Rs]) :- assert_rule(R), assert_rules(Rs).
assert_rules([]).

assert_rule(main :- Body) :-
    canonical2(Body, BodyC),
    assert(main :- BodyC).
assert_rule(Head :- Body) :-
    canonical(Head, HeadC),
    canonical(Body, BodyC),
    varnumbers_names(HeadC :- BodyC, Term, _),
    assert(Term).
assert_rule(op(_)).


% tokenize
tokens(Ts) --> " ", tokens(Ts).
tokens(Ts) --> comment, !, tokens(Ts).
tokens([T|Ts]) --> tok(T), !, tokens(Ts).
tokens([]) --> "".

% # から行末までがコメント
comment --> "#", string(_), "\n", !.
string([]) --> [].
string([X|Xs]) --> [X], string(Xs).

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
alnum(T) --> [C], "'",
    { code_type(C, lower), atom_concat(C, '\'', CN), T = '$VAR'(CN) }.
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
rules([R | Rs]) --> skip(";"), rul(R), rules(Rs).
rules([]) --> skip(";").

%rul(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".
%rul(P :- true) --> pred(P), ";".
%rul(P :- B) --> pred(P), "{", skip(";"), body(B), "}".

notation([R]) --> [R], {not(R = ';')}.
notation([R|Rs]) --> [R], {not(R = ';')}, notation(Rs).

pred(O) --> e(0, O).

body(P) --> pred(P), skip(";").
body((P,Bs)) --> pred(P), ";", body(Bs).

% () はあらかじめ定義しておく
ops(a('()'), 0, leading, ['(',0,')']).

% (( )) は prolog を参照することにする
ops(a('<>'), 0, leading, ['<',0,'>']).

% Prolog Term としての標準記法に変換
canonical((B, Bs), (P, Ps)) :-
    canonical(B, P),
    canonical(Bs, Ps).
canonical(Term, Canonical) :-
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, canonical(Arg, Canonical)
    % <a> は a にするが、functor の先頭に _ をつけない
    ; Functor = '<>' -> [Arg] = Args, Arg = Canonical
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


canonical2((B, Bs), (P, Ps)) :-
    canonical2(B, P),
    canonical2(Bs, Ps).
canonical2(Term, Canonical) :-
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, canonical2(Arg, Canonical)
    % <a> は a にするが、functor の先頭に _ をつけない
    ; Functor = '<>' -> [Arg] = Args, Arg = Canonical
    % $VAR は、$VAR(n) だけ n にする。他はそのまま
    %; Functor = '$VAR' -> Canonical = Term
    ; Functor = '$VAR' -> ([Arg] = Args, not(member(Arg, [t,u,v,w,x,y,z])) -> Canonical = Arg; Canonical = Term)
    % それ以外は Functor の先頭に _ をつける
    ; atomic_concat('_', Functor, FunctorC),
                        canonicalList2(Args, ArgsC),
                        Canonical =.. [FunctorC | ArgsC]).  
canonical2(T, T).

canonicalList2([], []).
canonicalList2([A|As], [C|Cs]) :-
    canonical2(A,C),
    canonicalList2(As, Cs).

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
    clause(C, B),
    varnumbers_names(B, T, P), !,
    mi(T),
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

code_pred(Code, Pred) :-
    code_tokens(Code, Tokens),
    phrase(pred(Pred), Tokens).

code_pred_canonical(Code, C) :-
    code_pred(Code, Pred),
    canonical(Pred, C).
    


mi(true).
mi((A,B)) :-
        mi(A),
        mi(B).
mi(Goal) :- predicate_property(Goal,built_in), !, call(Goal).
mi(Goal) :-
        Goal \= true,
        Goal \= (_,_),
        %writeln(Goal),
        %sleep(0.1),
        clause(Goal, Body),
        mi(Body).