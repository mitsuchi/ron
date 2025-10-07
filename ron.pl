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

% ファイルを読み込んで評価する
file_eval(FilePath) :-
    read_file_to_string(FilePath, String, []), % ファイルを文字列にする
    string_chars(String, Chars),               % 文字列を文字リストにする
    chars_tokens(Chars, Tokens),               % 文字リストをトークンリストにする
    % トークンリストから演算子リストをパーズして残りトークンリストを得る
    tokens_ops(Ops, Tokens, RestTokens),
    % 演算子を Prolog の規則に登録して、残りのルール部分のトークンがパーズできるようにする
    maplist(assert_op, Ops), 
    % トークンリストから文法部分をパーズして文法リストを得る
    parse_syntax(Syntaxes, RestTokens, RestTokens2),
    % 文法リストから新たに登録するべきルールリストを作る
    syntaxes_rules(Syntaxes, RulesForSyntax), writeln(RulesForSyntax),
    % 残りのトークンからルール部分をパーズしてルールリストを得る
    tokens_rules(RestTokens2, Rules),
    % ルールを Prolog の規則に登録して、問い合わせを実行できるようにする    
    maplist(assert_rule, Rules),
    % 問い合わせを実行する
    query(main).

chars_tokens(Chars, Tokens) :-
    phrase(tokens(Tokens), Chars).

tokens_ops([R|Rs]) --> skip(";"), rule_op(R), !, tokens_ops(Rs).
tokens_ops([]) --> skip(";").

% 文法定義用の演算子を一時的に登録してパースし、その後削除する
parse_syntax(Syntaxes, RestTokens, RestTokens2) :-
    % 文法定義用の演算子を一時的に登録（他の演算子よりも低い優先順位）
    Ops = [ops(a('::='), -2, following, [-2,'::=',-2]),
           ops(a('|'), -1, following, [-1,'|',-1])],
    maplist(assert, Ops),
    % トークンリストから文法部分をパーズ
    tokens_syntaxes(Syntaxes, RestTokens, RestTokens2),
    % 文法定義用の演算子を削除
    maplist(retract, Ops).

% op ::= 'op' precedence ':' notation ';'
rule_op(op(Precedence, Notation)) --> [op], [Precedence], ":", notation(Notation), ";".

notation([R]) --> [R], {not(R = ';')}.
notation([R|Rs]) --> [R], {not(R = ';')}, notation(Rs).

% 任意の DCG ルールを 0回以上適用してリストを作る
sequence(_, []) --> [].
sequence(DCG, [X|Xs]) --> call(DCG, X), sequence(DCG, Xs).

% syntax ::= 'syntax' '{' (pred ';'?)* '}'
tokens_syntaxes(S) --> [syntax], skip(";"), "{", skip(";"), sequence(pred_skip, S), "}", skip(";").
tokens_syntaxes([]) --> skip(";").
% 述語を読んで改行をスキップする
pred_skip(P) --> pred(P), skip(";").

assert_op(op(Prec, ['_' | Ns])) :-
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores(Ns, Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    assert(Term).
assert_op(op(Prec, [N | Ns])) :-
    N \= '_',
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores([N|Ns], Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    assert(Term).
assert_op(_ :- _).

% 文法リストから新たに登録するべきルールリストを作る
% ex1: syntax { v ::= true | false } のとき、生成するルールは V true; V false; となる。そのとき、
% 文法リストは [ ::=($VAR(v),(true | false)) ] となり
% ルールリストは  [V(true) :- true, V(false) :- true] となる
% ex2: syntax { t ::= v | if t then t else t } のとき
% 生成するルールは T v1 { V v1 }; T(if t1 then t2 else t3) { T t1; T t2; T t3 } となる。そのとき、
% 文法リストは　[ ::=($VAR(t), ($VAR(v) | ifthenelse($VAR(t),$VAR(t),$VAR(t)))) ] となり
% ルールリストは [T($VAR(v1)):-V($VAR(v1)),T(()(ifthenelse($VAR(t1),$VAR(t2),$VAR(t3)))):-T($VAR(t1)),T($VAR(t2)),T($VAR(t3))] となる
% 要件: 文法リストの非終端記号は、ルールリストの述語では大文字にする( v -> V, t -> T)
syntaxes_rules(Syntaxes, RulesForSyntax) :-
    maplist(syntax_rules, Syntaxes, RulesLists),
    append(RulesLists, RulesForSyntax).

% 1つの文法定義からルールリストを作る
syntax_rules('::='('$VAR'(VarName), RHS), Rules) :-
    % 非終端記号を大文字に変換
    upcase_atom(VarName, UpperVarName),
    % 右辺を選択肢に分解
    alternatives(RHS, Alts),
    % 各選択肢に対してルールを作る
    maplist(alternative_rule(UpperVarName), Alts, Rules).

% | で区切られた選択肢をリストに分解
alternatives((A | B), [A|Rest]) :- !, alternatives(B, Rest).
alternatives(A, [A]).

% 選択肢からルールを作る
% アトムの場合: V(atom) :- true
alternative_rule(VarName, Alt, (Head :- true)) :-
    atom(Alt),
    Head =.. [VarName, Alt].
% 変数の場合: T($VAR(v1)) :- V($VAR(v1))
alternative_rule(VarName, '$VAR'(OtherVarName), (Head :- Body)) :-
    % 新しい変数名を生成
    atom_concat(OtherVarName, '1', NewVarName),
    upcase_atom(OtherVarName, UpperOtherVarName),
    NewVar = '$VAR'(NewVarName),
    Head =.. [VarName, NewVar],
    Body =.. [UpperOtherVarName, NewVar].
% 複合項の場合: T(ifthenelse($VAR(t1),$VAR(t2),$VAR(t3))) :- T($VAR(t1)), T($VAR(t2)), T($VAR(t3))
alternative_rule(VarName, Alt, (Head :- Body)) :-
    compound(Alt),
    Alt =.. [Functor | Args],
    % 引数ごとに新しい$VAR変数を作る
    length(Args, Len),
    numlist(1, Len, Nums),
    maplist(make_var_from_arg, Args, Nums, NewVars),
    % ヘッドを作る
    NewAlt =.. [Functor | NewVars],
    Head =.. [VarName, NewAlt],
    % ボディを作る（各引数に対して型チェック）
    maplist(make_body(VarName), NewVars, BodyList),
    list_to_conjunction(BodyList, Body).

% 引数から新しい$VAR変数を生成
% 引数が$VAR(name)の場合、$VAR(name + 番号)を生成
make_var_from_arg('$VAR'(ArgName), Num, '$VAR'(NewName)) :-
    atom_concat(ArgName, Num, NewName).
% 引数がその他の場合（将来の拡張用）
make_var_from_arg(_, Num, '$VAR'(NewName)) :-
    atom_concat('_', Num, NewName).

% ボディの各項を作る
make_body(VarName, Arg, Body) :-
    Body =.. [VarName, Arg].

% リストを , で結合した項に変換
list_to_conjunction([X], X) :- !.
list_to_conjunction([X|Xs], (X, Rest)) :-
    list_to_conjunction(Xs, Rest).

assert_rule(main :- Body) :-
    normalize_term(Body, BodyN, true),
    assert(main :- BodyN).
assert_rule(Head :- Body) :-
    normalize_term(Head, HeadN),
    normalize_term(Body, BodyN),
    varnumbers_names(HeadN :- BodyN, Term, _),
    assert(Term).
assert_rule(op(_)).

tokens_rules(Tokens, Rules) :-
    phrase(rules_pred(Rules), Tokens).

% rule ::= pred ';' | pred '{' body '}'
% body ::= pred ';' | pred ';' body
rules_pred([R | Rs]) --> skip(";"), rule_pred(R), rules_pred(Rs).
rules_pred([]) --> skip(";").

rule_pred(P :- true) --> pred(P), ";".
rule_pred(P :- B) --> pred(P), "{", skip(";"), body(B), "}".

body(P) --> pred(P), skip(";").
body((P,Bs)) --> pred(P), ";", body(Bs).

% 述語をパーズして結果の項を O に入れる
% 現在の優先順位を考えられる最低にして式をパーズする。::= が -2 だったりするので。
pred(O) --> e(-100, O).

% tokenize
tokens(Ts) --> " ", tokens(Ts).
tokens(Ts) --> comment, !, tokens(Ts).
tokens([T|Ts]) --> token(T), !, tokens(Ts).
tokens([]) --> "".

% # から行末までがコメント
comment --> "#", string(_), "\n", !.
string([]) --> [].
string([X|Xs]) --> [X], string(Xs).

token(N) --> num(N).
token(;) --> ['\n'].
token(;) --> ";".
token(Atom) --> puncts(Cs), {atom_chars(Atom, Cs)}.
token(Atom) --> word(Cs), {length(Cs, N), N > 1, atom_chars(Atom, Cs)}.
token(Atom) --> [C], {code_type(C, upper), atom_chars(Atom, [C])}.
token(Alnum) --> alnum(Alnum).
token(Alpha) --> alpha(Alpha).
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

skip(W) --> (W, skip(W)) | "".

% mixfix parser
% https://zenn.dev/pandaman64/books/pratt-parsing
% https://qiita.com/h_sakurai/items/40abccdaad4728e0602e

% 式をパーズして項をつくり O に入れる。現在の優先順位を P とする。
e(P,O)-->
    [U], 
    {is_expression_token(U)},
    t(P,U,O).
% 前置演算子の項をパーズして項をつくり O に入れる。現在の優先順位を P, 作成中の項を T とする。
t(P,T,O)-->
    % 種類を leading とする　
    {ops(A, _, leading, [L|M])},
    {L=T},     
    f(M,V),             
    {call(A,V,W)},      
    t(P,W,O).
% 中置演算子の項をパーズして項をつくり O に入れる。現在の優先順位を P, 作成中の項を T とする。
t(P,T,O)-->   
    [U],      
    {is_expression_token(U)},
    % 種類を following とする
    {ops(A, _, following, [L,L2|M])},
    {L2=U, P<L},        
    f(M,V),             
    {call(A,[T|V],W)},   
    t(P,W,O).            
% 関数適用 _ _ の項をパーズして項をつくり O に入れる。現在の優先順位を P, 作成中の項を T とする。
t(P,T,O)-->
    % 種類を following とする
    {ops(A, _, following, [L,L2|M])},
    {number(L2), P<L},
    g([L2|M],V),      
    {call(A,[T|V],W)},
    t(P,W,O)          
    ; {T=O}.          
% 演算子の項の残りをパーズして引数の項のリストをつくり R に入れる。
f([P|N],R)-->         
    (
        {number(P),!},
        e(P,T),       
        {R=[T|I]}     
        ;[P],         
        {R=I}),       
    f(N,I).           
f([],[])-->!.         
% f と同様だが関数適用の場合はこちらを使う
g([P|N],R)-->         
    e_function(P,T),
    {R=[T|I]},       
    g(N,I).            
g([],[])-->!.          
% 関数適用の左辺として式をパーズ
e_function(P,O)-->
    [U],     
    {is_expression_token(U)},
    {is_function_left_side(U)},
    t(P,U,O).

% P(T) を R とする。ex: if と [1,2,3] から if(1,2,3) を得る
a(P,T,R) :- R=..[P|T].

% 式を構成するトークンかどうか
is_expression_token(Token) :-
    not(Token = ';'),
    not(Token = '{'),
    not(Token = '}').

% 関数適用の左辺の最初の項として有効なトークンかどうか
is_function_left_side(Token) :-
    (number(Token) ; 
     variable(Token) ; 
     all_alpha(Token) ; 
     Token = '(' ; 
     Token = '{').

variable(U) :- U = '$VAR'(_).

all_alpha(U) :-
    not(U = '$VAR'(_)),
    atom_chars(U, Cs), all_alpha_chars(Cs).
all_alpha_chars([C|Cs]) :- 
    maplist(is_alpha_char, [C|Cs]).
is_alpha_char(Char) :-
    code_type(Char, alpha).

all_punct(U) :-
    not(U = '$VAR'(_)),
    atom_chars(U, Cs), all_punct_chars(Cs).
all_punct_chars([C|Cs]) :- 
    maplist(is_punct_char, [C|Cs]).
is_punct_char(Char) :-
    code_type(Char, punct).

% () はあらかじめ定義しておく
ops(a('()'), 0, leading, ['(',0,')']).

% (( )) は prolog を参照することにする
ops(a('<>'), 0, leading, ['<',0,'>']).

% 述語の各項に _ をつける : if(x,+(1,2),y) を _if(x,_+(1,2),y) に
normalize_term(Term, Normalized) :-
    normalize_term(Term, Normalized, false).
normalize_term((B, Bs), (P, Ps), SimplifyVars) :-
    normalize_term(B, P, SimplifyVars),
    normalize_term(Bs, Ps, SimplifyVars).
normalize_term(Term, Normalized, SimplifyVars) :-
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, normalize_term(Arg, Normalized, SimplifyVars)
    % <a> は a にするが、functor の先頭に _ をつけない
    ; Functor = '<>' -> [Arg] = Args, Arg = Normalized
    % $VAR の処理
    ; Functor = '$VAR' -> 
        (SimplifyVars = true ->
            ([Arg] = Args, not(member(Arg, [t,u,v,w,x,y,z])) -> Normalized = Arg; Normalized = Term)
        ;
            Normalized = Term
        )
    % それ以外は Functor の先頭に _ をつける
    ; atomic_concat('_', Functor, FunctorC),
                        normalize_list(Args, ArgsC, SimplifyVars),
                        Normalized =.. [FunctorC | ArgsC]).  
normalize_term(T, T, _).

normalize_list([], [], _).
normalize_list([A|As], [C|Cs], SimplifyVars) :-
    normalize_term(A, C, SimplifyVars),
    normalize_list(As, Cs, SimplifyVars).

replace_underscore_list(List, R, Result) :-
    maplist(replace_underscore(R), List, Result).
replace_underscore(R, '_', R).
replace_underscore(_, A, A) :- A \= '_'.

concat_without_underscores(List, Result) :-
    exclude(==('_'), List, Filtered),
    atomic_list_concat(Filtered, Result).

% 問い合わせ
query(C) :-
    clause(C, B),
    varnumbers_names(B, T, P), !,
    eval(T),
    (P \= [] -> unparse_answers(P) ; true).

query_string(S) :-
    code_pred_canonical(S, C),
    !,
    query(C).

unparse_answers([X = A|Ps]) :-
    format_term(A, F), atomics_to_string([X, =, F], ' ', UA), writeln(UA),
    unparse_answers(Ps).
unparse_answers([]).

% 結果を読みやすくする
% ex: _S(_S(Z)) を S S Z に
format_term(A, A) :- atom(A); number(A).
format_term(E, Str) :-
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    atom_concat('_', Op1, Op), 
    ops(a(Op1), _, _, As),
    format_each(Op1, As, Terms, Strs, false),
    atomic_list_concat(Strs, ' ', Str).

format_each(_, [], _, [], _).
format_each(Op, [A|As], [Term|Ts], [Str1|Rest], Paren) :-
    number(A),
    format_term_with_priority(Term, Op, Str1, Paren),
    format_each(Op, As, Ts, Rest, true).
format_each(Op, [A|As], Ts, [A|Rest], Paren) :-
    not(number(A)),
    format_each(Op, As, Ts, Rest, Paren).

format_term_with_priority(A, _, A, _) :- atom(A); number(A).
format_term_with_priority(E, Op1, Str, Paren) :-
    functor(E, _, _, compound),
    E =.. [Op | _],
    atom_concat('_', Op2, Op),
    ops(a(Op2), P2, _, _),
    ops(a(Op1), P1, _, _),
    ((P1 > P2 ; Paren, Op1 = Op2) -> format_term(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
            ; format_term(E, Str)).

code_pred(Code, Pred) :-
    chars_tokens(Code, Tokens),
    phrase(pred(Pred), Tokens).

code_pred_canonical(Code, C) :-
    code_pred(Code, Pred),
    normalize_term(Pred, C).
    
eval(true).
eval((A,B)) :-
        eval(A),
        eval(B).
eval(Goal) :- predicate_property(Goal,built_in), !, call(Goal).
eval(Goal) :-
        Goal \= true,
        Goal \= (_,_),
        clause(Goal, Body),
        eval(Body).