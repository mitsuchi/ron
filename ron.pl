:- set_prolog_flag(double_quotes, chars).
:- dynamic ops/4.
:- dynamic '_main'/0.

% メモ化用のキャッシュ
:- dynamic eval_cache/2.

:- initialization((run -> true ; (write('error'), nl, halt(1)))).

run :- 
    current_prolog_flag(argv, Argv),
    % デバッグモードをチェック
    (member('--debug', Argv) ->
        nb_setval(debug_mode, true),
        delete(Argv, '--debug', Argv2)
    ;
        nb_setval(debug_mode, false),
        Argv2 = Argv
    ),
    % 評価深さカウンタを初期化
    nb_setval(eval_depth, 0),
    nth1(1, Argv2, FilePath) ->
        (catch(
            file_eval(FilePath),
            Exception,
            (write('error: '), write(Exception), nl, halt(1))
        ), halt) ; true.

% デバッグ出力用のヘルパー述語
debug_print(Label, List) :-
    (nb_getval(debug_mode, true) ->
        writeln(Label),
        maplist(writeln, List)
    ;
        true
    ).

% ファイルを読み込んで評価する
file_eval(FilePath) :-
    % キャッシュをクリア
    retractall(eval_cache(_, _)),
    % ファイルを文字列にする
    read_file_to_string(FilePath, String, []),
    % 文字列を文字リストにする
    string_chars(String, Chars),
    % 文字リストをトークンリストにする
    chars_tokens(Chars, Tokens),
    % トークンリストから演算子リストをパーズして残りトークンリストと予約語リストを得る
    tokens_ops(Ops, ReservedWords, Tokens, RestTokens),
    % 演算子を Prolog の規則に登録して、残りのルール部分のトークンがパーズできるようにする
    maplist(assert_op, Ops), 
    % トークンリストから文法部分をパーズして文法リストを得る
    parse_syntax(Syntaxes, RestTokens, RestTokens2),
    % トークンリストから評価文脈部分をパーズして評価文脈リストを得る
    parse_context(Contexts, ContextRules, RestTokens2, RestTokens3),
    % 文法リストから予約語リストを得る
    syntaxes_reserved_words(Syntaxes, ReservedWords2),
    % 評価文脈リストから予約語リストを得る
    contexts_reserved_words(Contexts, ReservedWords3),
    % ReservedWords, ReservedWords2, ReservedWords3 を結合したうえでユニークにする。main も予約語とする
    merge_reserved_words_list([ReservedWords, ReservedWords2, ReservedWords3], AllReservedWords),
    % RestTokens3 のうち、[a-Z]+[0-9]*"'"* のパターンに一致するものを $VAR() に変換して RestTokens4 にする
    convert_vars_in_tokens(AllReservedWords, RestTokens3, RestTokens4),
    % ContextRules の変数も変換する必要がある
    convert_vars_in_context_rules(AllReservedWords, ContextRules, ContextRulesConverted),
    % 文法リストから新たに登録するべきルールリストを作る
    syntaxes_rules(Syntaxes, RulesForSyntax),
    % 評価文脈ルールを展開して具体的なルールリストを作る
    expand_context_rules(Contexts, ContextRulesConverted, RulesForContext),
    debug_print('RulesForContext:', RulesForContext),
    % 残りのトークンからルール部分をパーズしてルールリストを得る
    tokens_rules(RestTokens4, Rules),
    % 文法リストから非終端記号だけを抽出し、それをもとに既存のルールリストを更新して文法を満たすように条件を追加する
    update_rules(Syntaxes, Rules, UpdatedRules),
    % 評価文脈のルールも更新
    update_rules(Syntaxes, RulesForContext, UpdatedContextRules),
    % 文法用のルールリストを Prolog の規則に登録する
    maplist(assert_rule, RulesForSyntax),
    % 評価文脈のルールを Prolog の規則に登録する
    maplist(assert_rule, UpdatedContextRules),
    % 更新後のルールリストを Prolog の規則に登録する
    maplist(assert_rule, UpdatedRules),
    % main を問い合わせする
    query(main).

chars_tokens(Chars, Tokens) :-
    phrase(tokens(Tokens), Chars),
    debug_print('Tokens:', Tokens).

% 予約語リストをマージしてユニーク化（main も含める）
merge_reserved_words(ReservedWords1, ReservedWords2, MergedWords) :-
    append([main | ReservedWords1], ReservedWords2, TempWords),
    sort(TempWords, MergedWords),
    debug_print('AllReservedWords:', MergedWords).

% 複数の予約語リストをマージしてユニーク化（main も含める）
merge_reserved_words_list(ReservedWordsLists, MergedWords) :-
    append([[main] | ReservedWordsLists], TempWords),
    sort(TempWords, MergedWords),
    debug_print('AllReservedWords:', MergedWords).

% トークンリストのうち変数パターンに一致し予約語でないものを $VAR() に変換
convert_vars_in_tokens(ReservedWords, TokensIn, TokensOut) :-
    maplist(convert_token(ReservedWords), TokensIn, TokensOut).

% ContextRules の変数を変換
convert_vars_in_context_rules(ReservedWords, RulesIn, RulesOut) :-
    maplist(convert_vars_in_rule(ReservedWords), RulesIn, RulesOut).

convert_vars_in_rule(ReservedWords, (Head :- Body), (NewHead :- NewBody)) :-
    convert_vars_in_term(ReservedWords, Head, NewHead),
    convert_vars_in_term(ReservedWords, Body, NewBody).

convert_vars_in_term(ReservedWords, Term, '$VAR'(Term)) :-
    atom(Term),
    is_var_pattern(Term),
    \+ member(Term, ReservedWords),
    !.
convert_vars_in_term(ReservedWords, Term, Result) :-
    compound(Term),
    Term =.. [Functor | Args],
    maplist(convert_vars_in_term(ReservedWords), Args, NewArgs),
    Result =.. [Functor | NewArgs],
    !.
convert_vars_in_term(_, Term, Term).

% 1つのトークンを変換
convert_token(ReservedWords, Token, '$VAR'(Token)) :-
    atom(Token),
    is_var_pattern(Token),
    \+ member(Token, ReservedWords),
    !.
convert_token(_, Token, Token).

% トークンが変数パターン [a-Z][a-Z]?[0-9]*'* に一致するかチェック
is_var_pattern(Token) :-
    atom(Token),
    atom_chars(Token, Chars),
    phrase(var_pattern, Chars).

% 変数パターン: [a-Z][a-Z]?[0-9]*'*
var_pattern --> alpha(_), optional(alpha, _), many(digit, _), many(quote, _).



tokens_ops(Ops, ReservedWords) --> tokens_ops_impl(Ops, ReservedWordsNested), {flatten(ReservedWordsNested, ReservedWords), debug_print('ReservedWords:', ReservedWords)}.

tokens_ops_impl([R|Rs], [RWs|RWsRest]) -->
    skip(";"),
    rule_op_with_reserved(R, RWs),
    !,
    tokens_ops_impl(Rs, RWsRest).
tokens_ops_impl([], []) --> skip(";").

% 文法定義用の演算子を一時的に登録してパースし、その後削除する
parse_syntax(Syntaxes, RestTokens, RestTokens2) :-
    % 文法定義用の演算子を一時的に登録（他の演算子よりも低い優先順位）
    Ops = [ops(a('::='), -2, following, [-2,'::=',-2]),
           ops(a('|'), -1, following, [-1,'|',-1])],
    setup_call_cleanup(
        maplist(assertz, Ops),
        % トークンリストから文法部分をパーズ
        tokens_syntaxes(Syntaxes, RestTokens, RestTokens2),
        % 文法定義用の演算子を削除
        maplist(retract, Ops)
    ),
    debug_print('Syntaxes:', Syntaxes).

% 評価文脈定義用の演算子を一時的に登録してパースし、その後削除する
parse_context(Contexts, ContextRules, RestTokens, RestTokens2) :-
    % 評価文脈定義用の演算子を一時的に登録
    % [] の優先順位を -> よりも高く設定
    Ops = [ops(a('::='), -2, following, [-2,'::=',-2]),
           ops(a('|'), -1, following, [-1,'|',-1]),
           ops(a('[]'), 110, following, [110,'[',110,']'])],
    setup_call_cleanup(
        maplist(assertz, Ops),
        % トークンリストから評価文脈部分をパーズ
        (phrase(tokens_contexts(Contexts, ContextRules), RestTokens, RestTokens2) ->
            true
        ;
            % context ブロックがない場合
            Contexts = [],
            ContextRules = [],
            RestTokens2 = RestTokens
        ),
        % 評価文脈定義用の演算子を削除
        maplist(retract, Ops)
    ),
    debug_print('Contexts:', Contexts),
    debug_print('ContextRules:', ContextRules).



% op をパーズして予約語も抽出する
rule_op_with_reserved(op(Precedence, Notation), ReservedWords) -->
    [op], [Precedence], ":", notation(Notation), ";",
    {findall(Word, (member(Word, Notation), atom(Word), all_alpha(Word)), ReservedWords)}.

notation([R]) --> [R], {not(R = ';')}.
notation([R|Rs]) --> [R], {not(R = ';')}, notation(Rs).

% syntax ::= 'syntax' '{' (pred ';'?)* '}'
tokens_syntaxes(S) --> [syntax], skip_token(;), ['{'], skip_token(;),
    collect_until_brace(Tokens),
    {exclude(=(;), Tokens, TokensNoSemi)},
    {parse_syntax_list(TokensNoSemi, S)},
    skip_token(;).
tokens_syntaxes([]) --> skip_token(;).

% 文法リストをパーズする（::=の項のリストとして認識）
% トークンを ::= で分割してから、各部分をパーズする
parse_syntax_list(Tokens, Syntaxes) :-
    split_by_syntax_def(Tokens, Defs),
    maplist(parse_one_syntax, Defs, Syntaxes).

% トークンを ::= で分割する
split_by_syntax_def([], []).
split_by_syntax_def(Tokens, [Def|Defs]) :-
    append(Before, ['::='|After], Tokens),
    Before \= [],
    !,
    split_at_next_lhs(After, RHS, Rest),
    append(Before, ['::='|RHS], Def),
    split_by_syntax_def(Rest, Defs).
split_by_syntax_def(Tokens, [Tokens]) :-
    Tokens \= [].

% 次の左辺（非終端記号）まで分割
split_at_next_lhs([], [], []).
split_at_next_lhs([Token, '::='|Rest], [], [Token, '::='|Rest]) :-
    atom(Token),
    all_alpha(Token),
    !.
split_at_next_lhs([Token|Rest], [Token|RHS], Remaining) :-
    split_at_next_lhs(Rest, RHS, Remaining).

% 1つの文法定義をパーズする
parse_one_syntax(Tokens, '::='(LHS, RHS)) :-
    Tokens = [LHS, '::='|RHSTokens],
    atom(LHS),
    all_alpha(LHS),
    phrase(pred(RHS), RHSTokens).

% context ::= 'context' '{' (定義 | ルール)* '}'
% syntax と同じパターンでパース
tokens_contexts(Contexts, Rules) --> [context], skip_token(;), ['{'], skip_token(;),
    collect_until_brace(Tokens),
    {phrase(rules_pred(AllItems), Tokens)},
    {partition_contexts(AllItems, Contexts, Rules)},
    skip_token(;).
tokens_contexts([], []) --> skip_token(;).

% アイテムを文法定義とルールに分類
partition_contexts([], [], []).
% ::=(_, _) :- true の形式
partition_contexts([('::='(L, R) :- true)|Rest], ['::='(L, R)|Contexts], Rules) :- !,
    partition_contexts(Rest, Contexts, Rules).
% ::=(_, _) の形式（ボディなし）
partition_contexts([Item|Rest], [Item|Contexts], Rules) :-
    Item = '::='(_, _), !,
    partition_contexts(Rest, Contexts, Rules).
% それ以外はルール
partition_contexts([Item|Rest], Contexts, [Item|Rules]) :-
    partition_contexts(Rest, Contexts, Rules).

% 評価文脈ルールを展開する
% Contexts: 評価文脈定義のリスト（例：['::='(E, '|'('_'(_,+,e), '+'(v,_)))]）
% ContextRules: 簡約規則のリスト（例：[(E[e1] -> E[e2] :- e1 -> e2)]）
% ExpandedRules: 展開されたルールのリスト
expand_context_rules(Contexts, ContextRules, ExpandedRules) :-
    debug_print('Expanding contexts:', Contexts),
    debug_print('With rules:', ContextRules),
    findall(Rule,
            (member(ContextRule, ContextRules),
             member(ContextDef, Contexts),
             expand_one_context_rule(ContextDef, ContextRule, Rule)),
            ExpandedRulesNested),
    debug_print('ExpandedRulesNested:', ExpandedRulesNested),
    flatten(ExpandedRulesNested, ExpandedRules).

% 1つの評価文脈ルールを展開
% ContextDef: E ::= _ + e | v + _ の形式
% ContextRule: E [e1] op E [e2] { body } の形式（op は任意の演算子）
% Rules: 展開されたルールのリスト
expand_one_context_rule('::='(CtxName, RHS), (Head :- Body), Rules) :-
    % ルールのヘッドが E[e1] op E[e2] の形式かチェック
    % normalize 前なので任意の演算子と '[]' の形式
    compound(Head),
    Head =.. [Op, Left, Right],
    Left =.. ['[]', CtxName, E1],
    Right =.. ['[]', CtxName, E2],
    % 右辺を選択肢に分解
    alternatives(RHS, Alts),
    % 各選択肢についてルールを生成（失敗する選択肢はスキップ）
    findall(Rule, (member(Alt, Alts), expand_alternative(CtxName, E1, E2, Op, Body, Alt, Rule)), Rules).

% 1つの選択肢を具体的なルールに展開
% Alt: _ + e のような選択肢（n項演算子に対応）
% E1, E2: 穴に代入する項 : E [e1] -> E [e2] の e1 と e2
% Op: 簡約演算子（-> など）
% Body: 元のルールのボディ
% Rule: 展開されたルール
expand_alternative(CtxName, E1, E2, Op, Body, Alt, (NewHead :- NewBody)) :-
    % 左辺と右辺で同じ非終端記号を使う（評価文脈の周囲の式は変わらない）
    % 評価文脈名を渡して、再帰参照を検出
    replace_nonterminals_with_vars(CtxName, Alt, AltWithVars, _),
    % 穴だけの選択肢（_）はスキップ（これは簡約規則にならない）
    AltWithVars \= '_',
    % 穴 '_' を見つけて E1/E2 で置き換える（n項演算子に対応）
    substitute_holes(AltWithVars, E1, E2, LeftSide, RightSide),
    % 新しいヘッドを作る：LeftSide Op RightSide
    % normalize_term で処理されるように、正規化前の形式で作成
    NewHead =.. [Op, LeftSide, RightSide],
    % ボディを置き換える: E1 を E1' のような新しい変数に変換
    rename_term_in_body(Body, E1, E2, NewBody).

% 評価文脈定義内の非終末記号を新しい変数に置き換える
% 例: _ + e  →  _ + $VAR(e10)
% 各非終端記号は別々の変数として扱う（同じ名前でもカウンタで区別）
% CtxName: 評価文脈名（再帰参照を検出するため）
replace_nonterminals_with_vars(CtxName, Term, Result, VarMap) :-
    replace_nonterminals_impl(CtxName, Term, Result, [], VarMap).

replace_nonterminals_impl(_CtxName, '_', '_', VarMap, VarMap) :- !.
replace_nonterminals_impl(CtxName, Atom, Result, VarMapIn, VarMapOut) :-
    atom(Atom),
    Atom \= '_',
    % 評価文脈名自身への参照は穴として扱う（再帰的定義をサポート）
    (Atom = CtxName ->
        Result = '_',
        VarMapOut = VarMapIn
    % 小文字のアトムは非終端記号の可能性が高い
    ; all_alpha(Atom) ->
        % 同じ名前でもカウントを増やして新しい変数を生成
        % カウンタは10から始める（e1, e2 などとぶつからないように）
        count_occurrences(Atom, VarMapIn, Count),
        NextCount is Count + 10,
        atom_concat(Atom, NextCount, VarName),
        Result = '$VAR'(VarName),
        VarMapOut = [(Atom, Result) | VarMapIn]
    ;
        Result = Atom,
        VarMapOut = VarMapIn
    ), !.
replace_nonterminals_impl(CtxName, Term, Result, VarMapIn, VarMapOut) :-
    compound(Term),
    Term =.. [Functor | Args],
    maplist_with_state_ctx(CtxName, Args, NewArgs, VarMapIn, VarMapOut),
    Result =.. [Functor | NewArgs], !.
replace_nonterminals_impl(_CtxName, Term, Term, VarMap, VarMap).

% 指定されたアトムの出現回数をカウント
count_occurrences(Atom, VarMap, Count) :-
    findall(1, member((Atom, _), VarMap), Ones),
    length(Ones, Count).

% maplist with state accumulator (context name を保持)
maplist_with_state_ctx(_, [], [], State, State).
maplist_with_state_ctx(CtxName, [X|Xs], [Y|Ys], StateIn, StateOut) :-
    replace_nonterminals_impl(CtxName, X, Y, StateIn, StateMid),
    maplist_with_state_ctx(CtxName, Xs, Ys, StateMid, StateOut).

% 複数の穴 '_' を指定された項で置き換える（n項演算子に対応）
% 左辺では E1 で、右辺では E2 で置き換える
substitute_holes(Term, E1, E2, LeftResult, RightResult) :-
    substitute_holes_impl(Term, E1, LeftResult),
    substitute_holes_impl(Term, E2, RightResult).

% 穴 '_' を指定された項で置き換える（内部実装）
substitute_holes_impl('_', Replacement, Replacement) :- !.
substitute_holes_impl(Term, Replacement, Result) :-
    compound(Term),
    Term =.. [Functor | Args],
    maplist(substitute_holes_arg(Replacement), Args, NewArgs),
    Result =.. [Functor | NewArgs], !.
substitute_holes_impl(Term, _, Term).

substitute_holes_arg(Replacement, '_', Replacement) :- !.
substitute_holes_arg(Replacement, Term, Result) :-
    compound(Term),
    Term =.. [Functor | Args],
    maplist(substitute_holes_arg(Replacement), Args, NewArgs),
    Result =.. [Functor | NewArgs], !.
substitute_holes_arg(_, Term, Term).

% ボディ内の変数名を置き換える
rename_term_in_body(Body, _E1, _E2, Body) :-
    % E1 と E2 は既に適切に名前が付けられているので、そのまま使う
    true.

% トークンを0回以上スキップ
skip_token(T) --> [T], skip_token(T).
skip_token(_) --> [].
% '}' まで全てのトークンを収集（ネストした {} も考慮）
collect_until_brace(Tokens) --> collect_until_brace_impl(0, 0, Tokens).

% Depth1: {} のネスト, Depth2: [] のネスト
collect_until_brace_impl(0, 0, []) --> ['}'], !.
collect_until_brace_impl(Depth1, Depth2, [T|Ts]) --> [T],
    {(T = '{' -> NewDepth1 is Depth1 + 1, NewDepth2 = Depth2
     ; T = '}' -> NewDepth1 is Depth1 - 1, NewDepth2 = Depth2
     ; T = '[' -> NewDepth1 = Depth1, NewDepth2 is Depth2 + 1
     ; T = ']' -> NewDepth1 = Depth1, NewDepth2 is Depth2 - 1
     ; NewDepth1 = Depth1, NewDepth2 = Depth2)},
    collect_until_brace_impl(NewDepth1, NewDepth2, Ts).

assert_op(op(Prec, ['_' | Ns])) :-
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores(Ns, Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    assertz(Term).
assert_op(op(Prec, [N | Ns])) :-
    N \= '_',
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores([N|Ns], Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    assertz(Term).
assert_op(_ :- _).

% 文法リストから新たに登録するべきルールリストを作る
syntaxes_rules(Syntaxes, RulesForSyntax) :-
    % 全ての非終端記号を抽出
    extract_nonterminals(Syntaxes, Nonterminals),
    % 各文法定義に非終端記号リストを渡してルールを生成
    maplist(syntax_rules(Nonterminals), Syntaxes, RulesLists),
    append(RulesLists, RulesForSyntax),
    debug_print('RulesForSyntax:', RulesForSyntax).

% 1つの文法定義からルールリストを作る
syntax_rules(Nonterminals, '::='(VarName, RHS), Rules) :-
    atom(VarName),
    % 非終端記号を大文字に変換
    upcase_atom(VarName, UpperVarName),
    % 右辺を選択肢に分解
    alternatives(RHS, Alts),
    % 各選択肢に対してルールを作る
    maplist(alternative_rule(Nonterminals, UpperVarName), Alts, Rules).

% | で区切られた選択肢をリストに分解（入れ子の | もフラット化する）
alternatives(Term, Alts) :-
    alternatives_impl(Term, Alts).

alternatives_impl(Term, Alts) :-
    compound(Term),
    Term =.. ['|', A, B],
    !,
    alternatives_impl(A, AltsA),
    alternatives_impl(B, AltsB),
    append(AltsA, AltsB, Alts).
alternatives_impl(A, [A]).

% 選択肢からルールを作る
% アトムの場合で、他の非終端記号でない場合: V(atom) :- true
alternative_rule(Nonterminals, UpperVarName, Alt, (Head :- true)) :-
    atom(Alt),
    \+ member(Alt, Nonterminals),
    Head =.. [UpperVarName, Alt].
% アトムの場合で、他の非終端記号への参照の場合: T($VAR(v1)) :- V($VAR(v1))
alternative_rule(Nonterminals, UpperVarName, OtherVarName, (Head :- Body)) :-
    atom(OtherVarName),
    member(OtherVarName, Nonterminals),
    % 新しい変数名を生成
    atom_concat(OtherVarName, '1', NewVarName),
    upcase_atom(OtherVarName, UpperOtherVarName),
    NewVar = '$VAR'(NewVarName),
    Head =.. [UpperVarName, NewVar],
    Body =.. [UpperOtherVarName, NewVar].
% <述語> の形式（引数なし）：N($VAR(n1)) :- <>(integer($VAR(n1)))
% ボディに <> を保持することで、normalize_term で integer が _integer にならない
alternative_rule(_Nonterminals, UpperVarName, '<>'(PredicateName), (Head :- Body)) :-
    atom(PredicateName),
    % 非終端記号名から変数名を生成（UpperVarName を小文字化して使用）
    downcase_atom(UpperVarName, LowerVarName),
    atom_concat(LowerVarName, '1', NewVarName),
    NewVar = '$VAR'(NewVarName),
    % ヘッドを作る
    Head =.. [UpperVarName, NewVar],
    % ボディはPrologの述語呼び出し（<> で囲む）
    Call =.. [PredicateName, NewVar],
    Body = '<>'(Call).
% 複合項の場合: T(ifthenelse($VAR(t1),$VAR(t2),$VAR(t3))) :- T($VAR(t1)), T($VAR(t2)), T($VAR(t3))
alternative_rule(Nonterminals, UpperVarName, Alt, (Head :- Body)) :-
    compound(Alt),
    Alt =.. [Functor | Args],
    % 引数ごとに新しい$VAR変数を作る
    length(Args, Len),
    numlist(1, Len, Nums),
    maplist(make_var_from_arg, Args, Nums, NewVars),
    % ヘッドを作る
    NewAlt =.. [Functor | NewVars],
    Head =.. [UpperVarName, NewAlt],
    % ボディを作る（各引数に対して型チェック）
    maplist(make_body_for_arg(Nonterminals), Args, NewVars, BodyList),
    list_to_conjunction(BodyList, Body).
% 数値の場合: V(0) :- true
alternative_rule(_, UpperVarName, Alt, (Head :- true)) :-
    number(Alt),
    Head =.. [UpperVarName, Alt].

% 引数から新しい$VAR変数を生成
% 引数が単なるアトム（非終端記号への参照）の場合、$VAR(name + 番号)を生成
make_var_from_arg(ArgName, Num, '$VAR'(NewName)) :-
    atom(ArgName),
    atom_concat(ArgName, Num, NewName).

% ボディの各項を作る（元の引数と新しい変数から）
make_body_for_arg(Nonterminals, ArgName, NewVar, Body) :-
    atom(ArgName),
    member(ArgName, Nonterminals),
    upcase_atom(ArgName, UpperArgName),
    Body =.. [UpperArgName, NewVar].

% リストを , で結合した項に変換
list_to_conjunction([X], X) :- !.
list_to_conjunction([X|Xs], (X, Rest)) :-
    list_to_conjunction(Xs, Rest).

% 文法リストから非終端記号だけを抽出し、それをもとに既存のルールリストを更新して文法を満たすように条件を追加する
update_rules(Syntaxes, Rules, UpdatedRules) :-
    % 文法リストから非終端記号を抽出
    extract_nonterminals(Syntaxes, Nonterminals),
    % 各ルールを更新
    maplist(update_rule(Nonterminals), Rules, UpdatedRules),
    debug_print('UpdatedRules:', UpdatedRules).

% 文法リストから非終端記号を抽出
extract_nonterminals(Syntaxes, Nonterminals) :-
    maplist(extract_nonterminal, Syntaxes, Nonterminals).

extract_nonterminal('::='(VarName, _), VarName) :-
    atom(VarName).

% 文法リストから予約語リストを得る
syntaxes_reserved_words(Syntaxes, ReservedWords2) :-
    extract_left_words(Syntaxes, LeftWords),
    extract_right_words(Syntaxes, RightWords),
    % RightWords - LeftWords の差集合を計算
    subtract(RightWords, LeftWords, ReservedWords2),
    debug_print('ReservedWords2:', ReservedWords2).

% 評価文脈リストから予約語リストを得る
% 評価文脈では、左辺（評価文脈名）のみを予約語とする
% 右辺の非終端記号は syntax で定義されているので、ここでは追加しない
contexts_reserved_words(Contexts, ReservedWords3) :-
    extract_left_words(Contexts, LeftWords),
    % 評価文脈名（左辺）のみを予約語とする
    ReservedWords3 = LeftWords,
    debug_print('ReservedWords3:', ReservedWords3).

% 文法リストの左辺から all_alpha を満たすアトムを抽出
extract_left_words(Syntaxes, LeftWords) :-
    findall(Left,
            (member('::='(Left, _), Syntaxes),
             atom(Left),
             all_alpha(Left)),
            LeftWords).

% 文法リストの右辺から all_alpha を満たすアトムを再帰的に抽出してユニーク化
extract_right_words(Syntaxes, RightWords) :-
    findall(Word,
            (member('::='(_, Right), Syntaxes),
             collect_all_alpha_terms(Right, Words),
             member(Word, Words)),
            AllWords),
    sort(AllWords, RightWords).  % sort でユニーク化

% 項を再帰的に走査して all_alpha を満たすアトムをすべて収集
collect_all_alpha_terms(Term, Words) :-
    atom(Term),
    all_alpha(Term),
    !,
    Words = [Term].
collect_all_alpha_terms(Term, Words) :-
    compound(Term),
    Term =.. [Functor | Args],
    % ファンクターをチェック
    (all_alpha(Functor) -> FunctorWords = [Functor] ; FunctorWords = []),
    % 引数を再帰的に処理
    maplist(collect_all_alpha_terms, Args, ArgWordsList),
    append([FunctorWords | ArgWordsList], Words),
    !.
collect_all_alpha_terms(_, []).

% 1つのルールを更新して型チェックを追加
update_rule(Nonterminals, (Head :- Body), (Head :- NewBody)) :-
    % ルール内の変数を収集
    collect_vars_in_term(Head, VarsInHead),
    collect_vars_in_term(Body, VarsInBody),
    append(VarsInHead, VarsInBody, AllVarsWithDup),
    % 重複を除去
    sort(AllVarsWithDup, AllVars),
    % 非終端記号に対応する変数から型チェックを生成
    findall(TypeCheck,
            (member('$VAR'(VarName), AllVars),
             member(NTName, Nonterminals),
             atom_concat(NTName, _, VarName),  % 変数名が非終端記号で始まる
             upcase_atom(NTName, UpperNTName),
             TypeCheck =.. [UpperNTName, '$VAR'(VarName)]),
            TypeChecks),
    % ボディに型チェックを追加
    (TypeChecks = [] ->
        NewBody = Body
    ;
        list_to_conjunction(TypeChecks, TypeCheckConj),
        (Body = true ->
            NewBody = TypeCheckConj
        ;
            NewBody = (Body, TypeCheckConj)
        )
    ).

% 項内の$VAR変数を収集
collect_vars_in_term('$VAR'(Name), ['$VAR'(Name)]) :- !.
collect_vars_in_term(Term, Vars) :-
    compound(Term),
    Term =.. [_ | Args],
    maplist(collect_vars_in_term, Args, VarLists),
    append(VarLists, Vars), !.
collect_vars_in_term(_, []).

assert_rule(main :- Body) :-
    normalize_term(Body, BodyN, true),
    assertz(main :- BodyN).
assert_rule(Head :- Body) :-
    normalize_term(Head, HeadN),
    normalize_term(Body, BodyN),
    varnumbers_names(HeadN :- BodyN, Term, _),
    assertz(Term).
assert_rule(op(_)).

tokens_rules(Tokens, Rules) :-
    phrase(rules_pred(Rules), Tokens),
    debug_print('Rules:', Rules).

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
tokens([;|Ts]) --> comment, !, tokens(Ts).
tokens([T|Ts]) --> token(T), !, tokens(Ts).
tokens([]) --> "".

% # から行末までがコメント（改行も消費して ; トークンに置き換える）
comment --> "#", skip_until_newline.
skip_until_newline --> "\n", !.
skip_until_newline --> [C], {C \= '\n'}, skip_until_newline.

token(N) --> num(N).
% 区切り文字っぽい記号は一つずつ別のトークンとする
% たとえば (pred (succ 0)) の最後の )) が一つのトークンと見なされるのを避ける
token(;) --> ['\n'].
token(;) --> ";".
token('(') --> "(".
token(')') --> ")".
token(',') --> ",".
token(Atom) --> puncts(Cs), {atom_chars(Atom, Cs)}.
token(Atom) --> word(Cs), {length(Cs, N), N > 1, atom_chars(Atom, Cs)}.
token(Atom) --> [C], {code_type(C, upper), atom_chars(Atom, [C])}.
% ギリシャ文字1文字を定数として認識
token(Atom) --> [C], {is_greek_char(C), atom_chars(Atom, [C])}.
token(Var) --> var(Var).
num(N) --> digits(Cs), { number_chars(N, Cs) }.
word(Cs) --> many1(lower, Cs).
lower(C) --> [C], { code_type(C, lower) }.
% [a-Z]+[0-9]*"'"* のパターンを変数名候補として扱う
var(Name) --> many1(alpha, A), many(digit, N), many(quote, D),
   {append(A, N, AN), append(AN, D, AND), atom_chars(Name, AND)}.
alpha(C) --> [C], { code_type(C, alpha) }.
quote('\'') --> "'".
puncts(Cs) --> many1(punct, Cs).
punct(C) --> [C], { is_punct_or_symbol(C) }.
digits(Cs) --> many1(digit, Cs).
digit(C)   --> [C], { code_type(C, digit) }.

skip(W) --> (W, skip(W)) | "".

many1(CFG, [Elem|Rest]) --> call(CFG, Elem), many1(CFG, Rest).
many1(CFG, [Elem]) --> call(CFG, Elem).
many(CFG, [Elem|Rest]) --> call(CFG, Elem), many(CFG, Rest).
many(_CFG, []) --> [].
optional(CFG, [Elem]) --> call(CFG, Elem).
optional(_CFG, []) --> [].

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
    atom_chars(U, Cs), maplist(is_alpha_char, Cs).
is_alpha_char(Char) :-
    code_type(Char, alpha).

% ギリシャ文字かどうかを判定
is_greek_char(Char) :-
    char_code(Char, Code),
    % Greek and Coptic block (U+0370-U+03FF)
    Code >= 0x0370, Code =< 0x03FF.



% 句読点または数学記号かどうかを判定
% 通常の ASCII 句読点に加えて、Unicode の数学記号（矢印など）も認識する
is_punct_or_symbol(Char) :-
    code_type(Char, punct).
is_punct_or_symbol(Char) :-
    % Unicode の矢印記号や数学記号を認識
    char_code(Char, Code),
    (
        % 矢印記号 (U+2190 - U+21FF)
        (Code >= 0x2190, Code =< 0x21FF) ;
        % 数学演算子 (U+2200 - U+22FF)
        (Code >= 0x2200, Code =< 0x22FF) ;
        % その他の数学記号 (U+2A00 - U+2AFF)
        (Code >= 0x2A00, Code =< 0x2AFF) ;
        % 補助的な数学演算子 (U+2900 - U+297F)
        (Code >= 0x2900, Code =< 0x297F)
    ).

% () はあらかじめ定義しておく
ops(a('()'), 0, leading, ['(',0,')']).

% <> は prolog の述語を参照することにする（優先順位を ::= より高くする）
ops(a('<>'), -10, leading, ['<',-10,'>']).

% 述語の各項に _ をつける : if(x,+(1,2),y) を _if(x,_+(1,2),y) に
normalize_term(Term, Normalized) :-
    normalize_term(Term, Normalized, false).
normalize_term((B, Bs), (P, Ps), SimplifyVars) :-
    normalize_term(B, P, SimplifyVars),
    normalize_term(Bs, Ps, SimplifyVars).
normalize_term(Term, Normalized, SimplifyVars) :-
    nonvar(Term),
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % (a) は a にする
    (Functor = '()' -> [Arg] = Args, normalize_term(Arg, Normalized, SimplifyVars)
    % <a> は a にするが、functor の先頭に _ をつけない
    % 中身を catch で囲んで、例外が発生したら失敗させる
    % これにより、is/2 が _+(1,2) のような複合項に対して失敗する
    ; Functor = '<>' -> [Arg] = Args, Normalized = catch(Arg, _, fail)
    % $VAR の処理
    ; Functor = '$VAR' -> 
        (SimplifyVars = true ->
            % main の中では、メタ変数のように見える単語もただのアトムとする
            % ただし t から z までの子文字一文字だけはメタ変数とする。それらは assert_rule で Prolog の変数になる
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



unparse_answers([X = A|Ps]) :-
    (nb_getval(debug_mode, true) ->
        (write('Formatting: '), write(X = A), nl)
    ; true),
    format_term(A, F),
    atomics_to_string([X, =, F], ' ', UA), writeln(UA),
    unparse_answers(Ps).
unparse_answers([]).

% 結果を読みやすくする
% ex: _S(_S(Z)) を S S Z に
format_term(A, A) :- atom(A); number(A).
format_term(A, '_') :- var(A), !.
format_term(E, Str) :-
    nonvar(E),
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
format_term_with_priority(A, _, '_', _) :- var(A), !.
format_term_with_priority(E, Op1, Str, Paren) :-
    nonvar(E),
    functor(E, _, _, compound),
    E =.. [Op | _],
    atom_concat('_', Op2, Op),
    ops(a(Op2), P2, _, _),
    ops(a(Op1), P1, _, _),
    ((P1 > P2 ; Paren, Op1 = Op2) -> format_term(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
            ; format_term(E, Str)).


    
eval(true).
eval((A,B)) :-
        eval(A),
        eval(B).
eval(Goal) :- predicate_property(Goal,built_in), !, call(Goal).
eval(Goal) :-
        Goal \= true,
        Goal \= (_,_),
        % 深さ制限をチェック
        nb_getval(eval_depth, Depth),
        (Depth >= 1000 ->
            throw('error: evaluation depth limit exceeded (possible infinite loop)')
        ;
            % 深さをインクリメント
            NewDepth is Depth + 1,
            nb_setval(eval_depth, NewDepth),
            % キャッシュを確認
            (eval_cache(Goal, Result) ->
                Result = true
            ;
                % 新規評価してキャッシュに保存
                clause(Goal, Body),
                eval(Body),
                assertz(eval_cache(Goal, true))
            ),
            % 深さをデクリメント
            nb_setval(eval_depth, Depth)
        ).