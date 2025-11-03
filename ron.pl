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
        select('--debug', Argv, Argv2)
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
    % トークンリストから use ディレクティブをパースして置き換え
    parse_use_directive(Tokens, TokensWithNewline),
    % トークンリストから演算子リストをパーズして残りトークンリストと予約語リストを得る
    tokens_ops(Ops, ReservedWords, TokensWithNewline, RestTokens),
    % 演算子を Prolog の規則に登録して、残りのルール部分のトークンがパーズできるようにする
    maplist(assert_op, Ops), 
    % トークンリストから文法部分をパーズして文法リストを得る
    parse_syntax(Syntaxes, RestTokens, RestTokens2),
    !,  % バックトラックを防ぐ
    % トークンリストから評価文脈部分をパーズして評価文脈リストを得る
    parse_context(Contexts, ContextRules, RestTokens2, RestTokens3),
    % 文法リストから予約語リストを得る
    syntaxes_reserved_words(Syntaxes, ReservedWords2),
    % 評価文脈リストから予約語リストを得る
    contexts_reserved_words(Contexts, ReservedWords3),
    % 非終端記号に対して op 0 : t? _ 形式の演算子を自動登録（変数変換前）
    register_syntax_operators(Syntaxes),
    % 非終端記号から t? 形式の予約語を生成して追加（t? は予約語として扱うため）
    extract_nonterminals(Syntaxes, Nonterminals),
    maplist(add_question_mark, Nonterminals, QuestionMarkNonterminals),
    % ReservedWords, ReservedWords2, ReservedWords3, QuestionMarkNonterminals を結合したうえでユニークにする。main も予約語とする
    merge_reserved_words_list([ReservedWords, ReservedWords2, ReservedWords3, QuestionMarkNonterminals], AllReservedWords),
    % RestTokens3 のうち、[a-Z]+[0-9]*"'"* のパターンに一致するものを $VAR() に変換して RestTokens4 にする
    convert_vars_in_tokens(AllReservedWords, RestTokens3, RestTokens4),
    % ContextRules の変数も変換する必要がある
    convert_vars_in_context_rules(AllReservedWords, ContextRules, ContextRulesConverted),
    % 文法リストから新たに登録するべきルールリストを作る
    syntaxes_rules(Syntaxes, RulesForSyntax),
    % 評価文脈ルールを展開して具体的なルールリストを作る
    expand_context_rules(Contexts, ContextRulesConverted, RulesForContext),
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
    % これより前にバックトラックしない
    !,
    % main を問い合わせする
    query(main) -> true ; (write('fail'), nl, halt(1)).

chars_tokens(Chars, Tokens) :-
    phrase(tokens(Tokens), Chars),
    debug_print('Tokens:', Tokens).

% use directive をパースして文字を置き換える
parse_use_directive(Tokens, Result) :-
    find_use_directives(Tokens, Replacements, RestTokens),
    Replacements \= [],
    !,
    replace_tokens(RestTokens, Replacements, Result),
    debug_print('TokensWithReplacements:', Result).
parse_use_directive(Tokens, Result) :-
    % use directive がない場合は ; を newline に、{ } を open/close に置き換え
    replace_tokens(Tokens, [(';', newline), ('{', open), ('}', close)], Result),
    debug_print('TokensWithNewline (default semicolon):', Result).

% 先頭行から連続するコメント行の直後にある use directive を探す
find_use_directives(Tokens, Replacements, RestTokens) :-
    find_use_directives_impl(Tokens, Replacements, RestTokens).

% 複数の use directive を探す
find_use_directives_impl(Tokens, Replacements, RestTokens) :-
    find_use_directives_impl(Tokens, [], Replacements, RestTokens).

find_use_directives_impl([newline | Rest], Acc, Replacements, RestTokens) :-
    find_use_directives_impl(Rest, Acc, Replacements, RestTokens).

% パターン1: use <token> for <target>
find_use_directives_impl([use, OldChar, for, NewChar, newline | Rest], Acc, Replacements, RestTokens) :-
    atom(OldChar), atom(NewChar),
    find_use_directives_impl(Rest, [(OldChar, NewChar)|Acc], Replacements, RestTokens).

% パターン2: use <token1> <token2> for block
find_use_directives_impl([use, OpenChar, CloseChar, for, block, newline | Rest], Acc, Replacements, RestTokens) :-
    atom(OpenChar), atom(CloseChar),
    find_use_directives_impl(Rest, [(CloseChar, close), (OpenChar, open)|Acc], Replacements, RestTokens).

% コメント行をスキップ
find_use_directives_impl(['#' | Rest], Acc, Replacements, RestTokens) :-
    skip_comment_line(Rest, AfterComment),
    find_use_directives_impl(AfterComment, Acc, Replacements, RestTokens).

% use ディレクティブの終了
find_use_directives_impl(Tokens, Acc, Replacements, Tokens) :-
    Tokens \= [newline|_],
    Tokens \= [use|_],
    Tokens \= ['#'|_],
    % use % for newline が指定されているかチェック
    (member(('%', newline), Acc) ->
        % use % for newline が指定されている場合は ; を newline に置き換えない
        (member((open, _), Acc) ; member((_, open), Acc) ->
            % 既に open が置き換えリストにある場合はそのまま
            reverse(Acc, Replacements)
        ;
            % open がない場合は { } を open/close に追加
            reverse([('}', close), ('{', open)|Acc], Replacements)
        )
    ;
        % use % for newline が指定されていない場合は ; を newline に置き換え
        (member((open, _), Acc) ; member((_, open), Acc) ->
            % 既に open が置き換えリストにある場合は ; を newline に追加
            reverse([(';', newline)|Acc], Replacements)
        ;
            % open がない場合は { } を open/close に追加、; を newline に追加
            reverse([('}', close), ('{', open), (';', newline)|Acc], Replacements)
        )
    ).

% コメント行をスキップ（# から newline まで）
skip_comment_line([newline | Rest], Rest).
skip_comment_line([_ | Rest], AfterComment) :-
    skip_comment_line(Rest, AfterComment).

% 複数のトークン置き換えを処理する
replace_tokens([], _, []).
replace_tokens([Token|Rest], Replacements, [NewToken|Result]) :-
    (member((Token, NewToken), Replacements) ->
        true
    ;
        NewToken = Token
    ),
    replace_tokens(Rest, Replacements, Result).

% 指定された文字を newline に置き換える（後方互換性のため）
replace_char_with_newline(Tokens, Char, Result) :-
    replace_tokens(Tokens, [(Char, newline)], Result).

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
    (nb_getval(debug_mode, true) -> (write('Converting token: '), write(Token), write(' -> $VAR('), write(Token), write(')'), nl) ; true),
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
    skip([newline]),
    rule_op_with_reserved(R, RWs),
    tokens_ops_impl(Rs, RWsRest).
tokens_ops_impl([], []) --> skip([newline]).

% 文法定義用の演算子を一時的に登録してパースし、その後削除する
parse_syntax(Syntaxes, RestTokens, RestTokens2) :-
    % 文法定義用の演算子を一時的に登録（他の演算子よりも低い優先順位）
    Ops = [ops(a('::='), -2, following, [-2,'::=',-2]),
           ops(a('|'), -1, following, [-1,'|',-1])],
    setup_call_cleanup(
        maplist(assertz, Ops),
        % トークンリストから文法部分をパーズ
        phrase(tokens_syntaxes(Syntaxes), RestTokens, RestTokens2),
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
        (
            % context syntax { ... } をパーズ
            (phrase(tokens_context_syntax(Contexts), RestTokens, RestTokens1) ->
                true
            ;
                % context syntax ブロックがない場合
                Contexts = [],
                RestTokens1 = RestTokens
            ),
            % context rule { ... } をパーズ
            (phrase(tokens_context_rule(ContextRules), RestTokens1, RestTokens2) ->
                true
            ;
                % context rule ブロックがない場合
                ContextRules = [],
                RestTokens2 = RestTokens1
            )
        ),
        % 評価文脈定義用の演算子を削除
        maplist(retract, Ops)
    ),
    debug_print('Contexts:', Contexts),
    debug_print_rules('ContextRules:', ContextRules).

% op をパーズして予約語も抽出する
rule_op_with_reserved(op(Precedence, right, Notation), ReservedWords) -->
    [op], [Precedence], [r], !, [:], notation(Notation), [newline],
    {findall(Word, (member(Word, Notation), atom(Word), all_alpha(Word)), ReservedWords1),
     append([op, r], ReservedWords1, ReservedWords)}.
rule_op_with_reserved(op(Precedence, left, Notation), ReservedWords) -->
    [op], [Precedence], [:], notation(Notation), [newline],
    {findall(Word, (member(Word, Notation), atom(Word), all_alpha(Word)), ReservedWords1),
     append([op], ReservedWords1, ReservedWords)}.

notation([R]) --> [R], {not(R = newline)}.
notation([R|Rs]) --> [R], {not(R = newline)}, notation(Rs).

% syntax ::= 'syntax' '{' (pred ';'?)* '}'
tokens_syntaxes(S) --> skip([newline]), [syntax], skip([newline]), [open], skip([newline]),
    collect_until_brace(Tokens),
    {exclude(=(newline), Tokens, TokensNoSemi)},
    {(parse_syntax_list(TokensNoSemi, S) ->
        true
    ;
        write('error: parse failed in syntax block (undefined operator or syntax error)'), nl,
        % syntaxブロック内のエラーを詳細に表示
        find_syntax_failing_point(TokensNoSemi, FailingInfo),
        write('Failing at: '), write(FailingInfo), nl,
        halt(1)
    )},
    skip([newline]).
tokens_syntaxes([]) --> [].

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

% syntaxブロック内の失敗箇所を特定する
find_syntax_failing_point(Tokens, FailingInfo) :-
    find_syntax_failing_point_impl(Tokens, [], FailingInfo).

find_syntax_failing_point_impl([], Acc, Acc).
find_syntax_failing_point_impl(Tokens, Acc, FailingInfo) :-
    % 文法定義を分割してチェック
    split_by_syntax_def(Tokens, Defs),
    find_first_failing_def(Defs, FailingDef),
    format_tokens_for_display(FailingDef, DisplayString),
    append(Acc, [DisplayString], FailingInfo).

% 最初に失敗する文法定義を見つける
find_first_failing_def([], []).
find_first_failing_def([Def|Rest], FailingDef) :-
    % 文法定義をパーズしてみる
    (parse_one_syntax(Def, _) ->
        % 成功した場合は次の定義をチェック
        find_first_failing_def(Rest, FailingDef)
    ;
        % 失敗した場合は、右辺の各部分をチェック
        find_failing_rhs_part(Def, FailingDef)
    ).

% 文法定義の右辺で失敗する部分を見つける
find_failing_rhs_part(Def, FailingPart) :-
    Def = [LHS, '::='|RHSTokens],
    % 右辺を | で分割
    split_rhs_by_pipe(RHSTokens, RHSLines),
    find_first_failing_rhs_line(RHSLines, FailingLine),
    % LHS ::= を前に付ける
    append([LHS, '::='], FailingLine, FailingPart).

% 右辺を | で分割する
split_rhs_by_pipe(Tokens, Lines) :-
    split_rhs_by_pipe_impl(Tokens, [], Lines).

split_rhs_by_pipe_impl([], Acc, Acc).
split_rhs_by_pipe_impl(Tokens, Acc, Lines) :-
    append(Line, ['|'|Rest], Tokens),
    !,
    append(Acc, [Line], NewAcc),
    split_rhs_by_pipe_impl(Rest, NewAcc, Lines).
split_rhs_by_pipe_impl(Tokens, Acc, Lines) :-
    append(Acc, [Tokens], Lines).

% 最初に失敗する右辺の行を見つける
find_first_failing_rhs_line([], []).
find_first_failing_rhs_line([Line|Rest], FailingLine) :-
    % 行をパーズしてみる
    (phrase(pred(_), Line) ->
        % 成功した場合は次の行をチェック
        find_first_failing_rhs_line(Rest, FailingLine)
    ;
        % 失敗した場合はこの行を返す
        FailingLine = Line
    ).

% 1つの文法定義をパーズする
parse_one_syntax(Tokens, '::='(LHS, RHS)) :-
    Tokens = [LHS, '::='|RHSTokens],
    atom(LHS),
    all_alpha(LHS),
    phrase(pred(RHS), RHSTokens).

% context syntax ::= 'context' 'syntax' '{' (pred ';'?)* '}'
% syntax ブロックと同じロジックを使用
tokens_context_syntax(S) --> 
    skip([newline]), 
    [context], skip([newline]), [syntax], skip([newline]), [open], skip([newline]),
    collect_until_brace(Tokens),
    {exclude(=(newline), Tokens, TokensNoSemi)},
    {(parse_syntax_list(TokensNoSemi, S) ->
        true
    ;
        write('error: parse failed in context syntax block (undefined operator or syntax error)'), nl,
        find_syntax_failing_point(TokensNoSemi, FailingInfo),
        write('Failing at: '), write(FailingInfo), nl,
        halt(1)
    )},
    skip([newline]).
tokens_context_syntax([]) --> [].

% context rule ::= 'context' 'rule' '{' ルール* '}'
% 通常のルールパーズを使用
tokens_context_rule(Rules) --> 
    skip([newline]), 
    [context], skip([newline]), [rule], skip([newline]), [open], skip([newline]),
    collect_until_brace(Tokens),
    {(phrase(rules_pred(Rules), Tokens) ->
        true
    ;
        write('error: parse failed in context rule block (undefined operator or syntax error)'), nl,
        halt(1)
    )},
    skip([newline]).
tokens_context_rule([]) --> [].

% 評価文脈ルールを展開する
% Contexts: 評価文脈定義のリスト（例：['::='(E, '|'('_'(_,+,e), '+'(v,_)))]）
% ContextRules: 簡約規則のリスト（例：[(E[e1] -> E[e2] :- e1 -> e2)]）
% ExpandedRules: 展開されたルールのリスト
expand_context_rules(Contexts, ContextRules, ExpandedRules) :-
    debug_print('Expanding contexts:', Contexts),
    debug_print_rules('With rules:', ContextRules),
    findall(Rule,
            (member(ContextRule, ContextRules),
             member(ContextDef, Contexts),
             expand_one_context_rule(ContextDef, ContextRule, Rule)),
            ExpandedRulesNested),
    debug_print_rules('ExpandedRulesNested:', ExpandedRulesNested),
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
collect_until_brace_impl(0, 0, []) --> [close], !.
collect_until_brace_impl(Depth1, Depth2, [T|Ts]) --> [T],
    {(T = open -> NewDepth1 is Depth1 + 1, NewDepth2 = Depth2
     ; T = close -> NewDepth1 is Depth1 - 1, NewDepth2 = Depth2
     ; T = '[' -> NewDepth1 = Depth1, NewDepth2 is Depth2 + 1
     ; T = ']' -> NewDepth1 = Depth1, NewDepth2 is Depth2 - 1
     ; NewDepth1 = Depth1, NewDepth2 = Depth2)},
    collect_until_brace_impl(NewDepth1, NewDepth2, Ts).

assert_op(op(Prec, right, ['_' | Ns])) :-
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores(Ns, Punct),
    % 右結合の場合、前方結合力を1増やす
    PrecRight is Prec + 1,
    Term = ops(a(Punct), Prec, following, [PrecRight|Ns_]),
    assertz(Term).
assert_op(op(Prec, left, ['_' | Ns])) :-
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores(Ns, Punct),
    Term = ops(a(Punct), Prec, following, [Prec|Ns_]),
    assertz(Term).
assert_op(op(Prec, right, [N | Ns])) :-
    N \= '_',
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores([N|Ns], Punct),
    % 右結合の場合、前方結合力を1増やす
    PrecRight is Prec + 1,
    % leading の場合、Ns_ の最初の数値を更新
    update_leading_precedence(Ns_, PrecRight, Ns_Updated),
    Term = ops(a(Punct), Prec, leading, [N|Ns_Updated]),
    assertz(Term).
assert_op(op(Prec, left, [N | Ns])) :-
    N \= '_',
    replace_underscore_list(Ns, Prec, Ns_),
    concat_without_underscores([N|Ns], Punct),
    Term = ops(a(Punct), Prec, leading, [N|Ns_]),
    assertz(Term).
assert_op(_ :- _).

% leading の場合の最初の数値を更新するヘルパー関数
update_leading_precedence([], _, []).
update_leading_precedence([H|T], NewPrec, [NewPrec|T]) :-
    number(H).
update_leading_precedence([H|T], NewPrec, [H|UpdatedT]) :-
    \+ number(H),
    update_leading_precedence(T, NewPrec, UpdatedT).

% 文法リストから新たに登録するべきルールリストを作る
syntaxes_rules(Syntaxes, RulesForSyntax) :-
    % 全ての非終端記号を抽出
    extract_nonterminals(Syntaxes, Nonterminals),
    % 各文法定義に非終端記号リストを渡してルールを生成
    maplist(syntax_rules(Nonterminals), Syntaxes, RulesLists),
    append(RulesLists, RulesFromSyntax),
    % 各非終端記号に対して t? t 形式のルールを生成
    maplist(create_question_mark_rule, Nonterminals, QuestionMarkRules),
    append(RulesFromSyntax, QuestionMarkRules, RulesForSyntax),
    debug_print_rules('RulesForSyntax:', RulesForSyntax).

% 非終端記号に対して t? t 形式のルールを作成
% 例: t?(Term) :- t(Term)
create_question_mark_rule(NT, (Head :- Body)) :-
    atom(NT),
    atom_concat(NT, '?', NTQuestion),
    % NTQuestion($VAR(TermName)) :- NT($VAR(TermName)) の形式
    atom_concat(NT, '1', TermName),
    TermVar = '$VAR'(TermName),
    Head =.. [NTQuestion, TermVar],
    Body =.. [NT, TermVar].

% 1つの文法定義からルールリストを作る
syntax_rules(Nonterminals, '::='(VarName, RHS), Rules) :-
    atom(VarName),
    % 非終端記号をそのまま使用（大文字変換を削除）
    % 右辺を選択肢に分解
    alternatives(RHS, Alts),
    % 各選択肢に対してルールを作る
    maplist(alternative_rule(Nonterminals, VarName), Alts, Rules).

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
alternative_rule(Nonterminals, VarName, OtherVarName, (Head :- Body)) :-
    atom(OtherVarName),
    member(OtherVarName, Nonterminals),
    % 新しい変数名を生成
    atom_concat(OtherVarName, '1', NewVarName),
    % 非終端記号をそのまま使用（大文字変換を削除）
    NewVar = '$VAR'(NewVarName),
    Head =.. [VarName, NewVar],
    Body =.. [OtherVarName, NewVar].
% <述語> の形式（引数なし）：N($VAR(n1)) :- <>(integer($VAR(n1)))
% ボディに <> を保持することで、normalize_term で integer が _integer にならない
alternative_rule(_Nonterminals, VarName, '<>'(PredicateName), (Head :- Body)) :-
    atom(PredicateName),
    % 非終端記号名から変数名を生成（VarName をそのまま使用）
    atom_concat(VarName, '1', NewVarName),
    NewVar = '$VAR'(NewVarName),
    % ヘッドを作る
    Head =.. [VarName, NewVar],
    % ボディはPrologの述語呼び出し（<> で囲む）
    Call =.. [PredicateName, NewVar],
    Body = '<>'(Call).
% 複合項の場合: T(ifthenelse($VAR(t1),$VAR(t2),$VAR(t3))) :- T($VAR(t1)), T($VAR(t2)), T($VAR(t3))
alternative_rule(Nonterminals, VarName, Alt, (Head :- Body)) :-
    compound(Alt),
    \+ is_dict(Alt),  % 辞書でないことを確認
    compound_name_arguments(Alt, Functor, Args),
    % 引数ごとに新しい$VAR変数を作る
    length(Args, Len),
    numlist(1, Len, Nums),
    maplist(make_var_from_arg, Args, Nums, NewVars),
    % ヘッドを作る
    compound_name_arguments(NewAlt, Functor, NewVars),
    Head =.. [VarName, NewAlt],
    % ボディを作る（各引数に対して型チェック）
    maplist(make_body_for_arg(Nonterminals), Args, NewVars, BodyList),
    list_to_conjunction(BodyList, Body).
% 数値の場合: V(0) :- true
alternative_rule(_, VarName, Alt, (Head :- true)) :-
    number(Alt),
    Head =.. [VarName, Alt].

% 引数から新しい$VAR変数を生成
% 引数が単なるアトム（非終端記号への参照）の場合、$VAR(name + 番号)を生成
make_var_from_arg(ArgName, Num, '$VAR'(NewName)) :-
    atom(ArgName),
    atom_concat(ArgName, Num, NewName).
% 数値の引数の場合：そのまま返す
make_var_from_arg(ArgNum, _Num, ArgNum) :-
    number(ArgNum).
% 複合項の引数の場合：再帰的に処理
make_var_from_arg(ArgTerm, _Num, NewVar) :-
    compound(ArgTerm),
    \+ is_dict(ArgTerm),  % 辞書でないことを確認
    compound_name_arguments(ArgTerm, Functor, Args),
    % 引数ごとに新しい$VAR変数を作る
    length(Args, Len),
    numlist(1, Len, Nums),
    maplist(make_var_from_arg, Args, Nums, NewArgs),
    % 新しい複合項を作る
    compound_name_arguments(NewVar, Functor, NewArgs).

% ボディの各項を作る（元の引数と新しい変数から）
make_body_for_arg(Nonterminals, ArgName, NewVar, Body) :-
    atom(ArgName),
    member(ArgName, Nonterminals),
    % 非終端記号をそのまま使用（大文字変換を削除）
    Body =.. [ArgName, NewVar].
% 数値の引数の場合：ボディは true
make_body_for_arg(_Nonterminals, ArgNum, _NewVar, true) :-
    number(ArgNum).
% 複合項の引数の場合：再帰的に処理
make_body_for_arg(Nonterminals, ArgTerm, _NewVar, Body) :-
    compound(ArgTerm),
    \+ is_dict(ArgTerm),  % 辞書でないことを確認
    compound_name_arguments(ArgTerm, _Functor, Args),
    % 引数ごとに新しい$VAR変数を作る
    length(Args, Len),
    numlist(1, Len, Nums),
    maplist(make_var_from_arg, Args, Nums, NewArgs),
    % ボディを作る（各引数に対して型チェック）
    maplist(make_body_for_arg(Nonterminals), Args, NewArgs, BodyList),
    list_to_conjunction(BodyList, Body).

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
    debug_print_rules('UpdatedRules:', UpdatedRules).

% 文法リストから非終端記号を抽出
extract_nonterminals(Syntaxes, Nonterminals) :-
    maplist(extract_nonterminal, Syntaxes, Nonterminals).

extract_nonterminal('::='(VarName, _), VarName) :-
    atom(VarName).

% 文法定義から非終端記号を抽出して op 0 : t? _ 形式の演算子を登録
register_syntax_operators(Syntaxes) :-
    extract_nonterminals(Syntaxes, Nonterminals),
    maplist(register_nonterminal_operator, Nonterminals),
    debug_print('Registered syntax operators for nonterminals:', Nonterminals).

% 非終端記号に対して op 0 : t? _ 形式の演算子を登録
register_nonterminal_operator(NT) :-
    atom(NT),
    % t? 形式のアトム名を作成
    atom_concat(NT, '?', NTQuestion),
    % op(0, left, [NTQuestion, '_']) 形式の演算子を登録
    assert_op(op(0, left, [NTQuestion, '_'])).

% 非終端記号に ? を追加して予約語リスト用のアトムを作成
add_question_mark(NT, NTQuestion) :-
    atom(NT),
    atom_concat(NT, '?', NTQuestion).

% 文法リストから予約語リストを得る
syntaxes_reserved_words(Syntaxes, ReservedWords2) :-
    extract_left_words(Syntaxes, LeftWords),
    (catch(extract_right_words(Syntaxes, RightWords), Error, (
        write('Error in extract_right_words: '), write(Error), nl,
        RightWords = []
    )) -> true ; RightWords = []),
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
    \+ is_dict(Term),  % 辞書でないことを確認
    compound_name_arguments(Term, Functor, Args),
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
    % 変数名にマッチする非終端記号の中から、最も長いものだけを選ぶ
    findall(TypeCheck,
            (member('$VAR'(VarName), AllVars),
             % 変数名が非終端記号で始まるものを全て見つける
             findall(NTName,
                     (member(NTName, Nonterminals),
                      atom_concat(NTName, _, VarName)),
                     MatchingNTs),
             % マッチする非終端記号がない場合はスキップ
             MatchingNTs \= [],
             % 最も長い非終端記号を選ぶ
             find_longest_prefix(Nonterminals, VarName, LongestNT),
             % 非終端記号をそのまま使用（大文字変換を削除）
             TypeCheck =.. [LongestNT, '$VAR'(VarName)]),
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

% 変数名にマッチする非終端記号の中から、最も長いものを選ぶ
find_longest_prefix(Nonterminals, VarName, LongestNT) :-
    % 変数名が非終端記号で始まるものを全て見つける
    findall(NTName,
            (member(NTName, Nonterminals),
             atom_concat(NTName, _, VarName)),
            MatchingNTs),
    MatchingNTs \= [],
    % 長さでソートして、最も長いものを選ぶ
    maplist(nt_name_length, MatchingNTs, MatchingNTsWithLength),
    sort(MatchingNTsWithLength, Sorted),
    reverse(Sorted, [_-LongestNT|_]).

% 非終端記号名とその長さのペアを作る
nt_name_length(NTName, Length-NTName) :-
    atom_length(NTName, Length).

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
    debug_print('Asserting main rule:', [main :- BodyN]),
    assertz(main :- BodyN).
assert_rule(Head :- Body) :-
    normalize_term(Head, HeadN),
    normalize_term(Body, BodyN),
    varnumbers_names(HeadN :- BodyN, Term, _),
    debug_print('Asserting rule:', [Term]),
    assertz(Term).
assert_rule(op(_)).

tokens_rules(Tokens, Rules) :-
    (phrase(rules_pred(Rules), Tokens) ->
        debug_print_rules('Rules:', Rules)
    ;
        write('error: parse failed (undefined operator or syntax error)'), nl,
        % より詳細なエラー情報を取得
        find_detailed_failing_point(Tokens, FailingInfo),
        write('Failing at: '), write(FailingInfo), nl,
        halt(1)
    ).

% より詳細な失敗箇所を特定する
find_detailed_failing_point(Tokens, FailingInfo) :-
    find_detailed_failing_point_impl(Tokens, [], FailingInfo).

find_detailed_failing_point_impl([], Acc, Acc).
find_detailed_failing_point_impl(Tokens, Acc, FailingInfo) :-
    % 次のルールを試行
    (phrase(rule_pred(_), Tokens, RestTokens) ->
        % 成功した場合、残りを処理
        find_detailed_failing_point_impl(RestTokens, Acc, FailingInfo)
    ;
        % 失敗した場合、現在の行（改行まで）を取得
        take_until_newline(Tokens, LineTokens),
        (LineTokens = [] ->
            % 行が空の場合は最初の数個のトークンを表示
            take_first_tokens(Tokens, 10, LineTokens)
        ;
            true
        ),
        % トークンリストを読みやすい形式に変換
        format_tokens_for_display(LineTokens, DisplayString),
        append(Acc, [DisplayString], FailingInfo)
    ).

% 失敗したルールを特定する
find_failing_rule(Tokens, FailingTokens) :-
    find_failing_rule_impl(Tokens, [], FailingTokens).

find_failing_rule_impl([], Acc, Acc).
find_failing_rule_impl(Tokens, Acc, FailingTokens) :-
    % 次のルールを試行
    (phrase(rule_pred(_), Tokens, RestTokens) ->
        % 成功した場合、残りを処理
        find_failing_rule_impl(RestTokens, Acc, FailingTokens)
    ;
        % 失敗した場合、現在の行（改行まで）を取得
        take_until_newline(Tokens, LineTokens),
        (LineTokens = [] ->
            % 行が空の場合は最初の数個のトークンを表示
            take_first_tokens(Tokens, 10, LineTokens)
        ;
            true
        ),
        append(Acc, LineTokens, FailingTokens)
    ).

% 改行までのトークンを取得（newline は含まない）
take_until_newline([], []).
take_until_newline([newline|_], []).
take_until_newline([Token|Rest], [Token|Result]) :-
    take_until_newline(Rest, Result).

% 最初の N 個のトークンを取得（newline はスキップ）
take_first_tokens([], _, []).
take_first_tokens([newline|Rest], N, Result) :-
    take_first_tokens(Rest, N, Result).
take_first_tokens([Token|Rest], N, [Token|Result]) :-
    N > 0,
    N1 is N - 1,
    take_first_tokens(Rest, N1, Result).
take_first_tokens(_, 0, []).

% トークンリストを読みやすい形式に変換
format_tokens_for_display(Tokens, DisplayString) :-
    % newline を除去
    exclude(==(newline), Tokens, TokensWithoutNewline),
    % 各トークンを文字列に変換
    maplist(format_single_token, TokensWithoutNewline, StringTokens),
    % 空白で結合
    atomic_list_concat(StringTokens, ' ', DisplayString).

% 単一トークンを文字列に変換
format_single_token('$VAR'(VarName), VarName) :- !.
format_single_token(open, '{') :- !.
format_single_token(close, '}') :- !.
format_single_token(Token, String) :-
    atom(Token),
    !,
    atom_string(Token, String).
format_single_token(Token, String) :-
    number(Token),
    !,
    number_string(Token, String).
format_single_token(Token, String) :-
    atom_string(Token, String).

% rule ::= pred ';' | pred '{' body '}'
% body ::= pred ';' | pred ';' body
% 述語を一つパーズしたらカットして後戻りしない
rules_pred([R | Rs]) --> skip([newline]), rule_pred(R), {debug_print('rules_pred: ', [R])}, !, rules_pred(Rs).
rules_pred([]) --> skip([newline]).

rule_pred(P :- true) --> pred(P), [newline].
rule_pred(P :- B) --> pred(P), [open], skip([newline]), body(B), [close].

body(P) --> pred(P), skip([newline]).
body((P,Bs)) --> pred(P), [newline], body(Bs).

% 述語をパーズして結果の項を O に入れる
% 現在の優先順位を考えられる最低にして式をパーズする。::= が -2 だったりするので。
pred(O) --> e(-100, O).

% tokenize
tokens(Ts) --> " ", tokens(Ts).
tokens([newline|Ts]) --> comment, !, tokens(Ts).
tokens([T|Ts]) --> token(T), !, tokens(Ts).
tokens([]) --> "".

% # から行末までがコメント（改行も消費して ; トークンに置き換える）
comment --> "#", skip_until_newline.
skip_until_newline --> "\n", !.
skip_until_newline --> [C], {C \= '\n'}, skip_until_newline.

token(N) --> num(N).
% 区切り文字っぽい記号は一つずつ別のトークンとする
% たとえば (pred (succ 0)) の最後の )) が一つのトークンと見なされるのを避ける
token(newline) --> ['\n'].
token(;) --> ";".
token('(') --> "(".
token(')') --> ")".
token('{') --> "{".
token('}') --> "}".
token(',') --> ",".
% ギリシャ文字1文字を定数として認識（変数トークンより先に扱う）
token(Atom) --> [C], {is_greek_char(C), atom_chars(Atom, [C])}.
% 英字列+数字+?+アポストロフィ列（例: ts', x10''）を1トークンとして扱う
token(Var) --> var(Var).
% 2文字以上の英字列は単語として扱う（変数トークンより後）
token(Atom) --> word(Cs), {length(Cs, N), N > 1, atom_chars(Atom, Cs)}.
% 連続する句読点は1トークン
token(Atom) --> puncts(Cs), {atom_chars(Atom, Cs)}.
num(N) --> digits(Cs), { number_chars(N, Cs) }.
word(Cs) --> many1(lower, Cs).
lower(C) --> [C], { code_type(C, lower) }.
% [a-Z]+[0-9]*"?"?"'"* のパターンを変数名候補として扱う
var(Name) --> many1(alpha, A), many(digit, N), optional(question, Q), many(quote, D),
   {append(A, N, AN), append(AN, Q, ANQ), append(ANQ, D, AND), atom_chars(Name, AND)}.
alpha(C) --> [C], { code_type(C, alpha) }.
quote('\'') --> "'".
question('?') --> "?".
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
    not(Token = newline),
    not(Token = open),
    not(Token = close).

% 関数適用の左辺の最初の項として有効なトークンかどうか
is_function_left_side(Token) :-
    (number(Token) ; 
     variable(Token) ; 
     all_alpha(Token) ; 
     Token = '(' ; 
     Token = '{' ;
     Token = '_' ;
     Token = open).

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
    % 中身を再帰的に正規化してから catch で囲む
    ; Functor = '<>' ->
        [Arg] = Args,
        normalize_term_in_catch(Arg, ArgN, SimplifyVars),
        Normalized = catch(ArgN, _, fail)
    % $VAR の処理
    ; Functor = '$VAR' -> 
        (SimplifyVars = true ->
            % main の中では、メタ変数のように見える単語もただのアトムとする
            % ただし [t-z] か [T-Z] までの一文字だけはメタ変数とする。それらは assert_rule で Prolog の変数になる
            ([Arg] = Args, not(member(Arg, [t,u,v,w,x,y,z,'T','U','V','W','X','Y','Z'])) -> Normalized = Arg; Normalized = Term)
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

% <>内の項を正規化する際の特別処理
% ()内のfreeinだけに_をつける
normalize_term_in_catch(Term, Normalized, SimplifyVars) :-
    nonvar(Term),
    functor(Term, _, _, compound),
    Term =.. [Functor | Args],
    % ()内の項を特別処理
    (Functor = '()' ->
        [Arg] = Args,
        normalize_term(Arg, ArgN, SimplifyVars),
        Normalized = ArgN
    % タプルの場合、各要素を個別に処理
    ; Functor = ',' ->
        [A, B] = Args,
        normalize_term_in_catch(A, AN, SimplifyVars),
        normalize_term_in_catch(B, BN, SimplifyVars),
        Normalized = (AN, BN)
    % 空のファンクター（タプル）の場合
    ; Functor = '' ->
        [A, B] = Args,
        normalize_term_in_catch(A, AN, SimplifyVars),
        normalize_term_in_catch(B, BN, SimplifyVars),
        Normalized = (AN, BN)
    % その他の複合項の場合、引数を再帰的に処理
    ; maplist(normalize_term_in_catch_wrapper(SimplifyVars), Args, ArgsN),
      Normalized =.. [Functor | ArgsN]).
normalize_term_in_catch(Term, Term, _).

% maplist用のラッパー
normalize_term_in_catch_wrapper(SimplifyVars, Arg, ArgN) :-
    normalize_term_in_catch(Arg, ArgN, SimplifyVars).

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
% タプル (comma ツリー) をカンマ区切りに整形
format_term(T, Str) :-
    nonvar(T),
    (   T = (_, _)
    ;   functor(T, '', 2)
    ),
    !,
    tuple_to_list(T, Elems),
    maplist(format_term, Elems, Strs),
    atomic_list_concat(Strs, ' , ', Str).
format_term(E, Str) :-
    nonvar(E),
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    atom_concat('_', Op1, Op), 
    ops(a(Op1), _, _, As),
    format_each(Op1, As, Terms, Strs, false),
    atomic_list_concat(Strs, ' ', Str).

% (A,B,...) のカンマ連結ツリーをフラットなリストに変換
tuple_to_list((A,B), List) :-
    !,
    tuple_to_list(A, L1),
    tuple_to_list(B, L2),
    append(L1, L2, List).
tuple_to_list(T, [T]).

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
% タプルはそのまま整形（括弧付けは format_term に任せる）
format_term_with_priority(T, _, Str, _) :-
    nonvar(T),
    (   T = (_, _)
    ;   functor(T, '', 2)
    ),
    !,
    format_term(T, Str).
format_term_with_priority(E, Op1, Str, Paren) :-
    nonvar(E),
    functor(E, _, _, compound),
    E =.. [Op | _],
    (atom_concat('_', Op2, Op) ->
        ops(a(Op2), P2, _, _),
        ops(a(Op1), P1, _, _),
        % 結合性を考慮した括弧付け
        (should_add_parens(Op1, P1, Op2, P2, Paren) ->
            format_term(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
        ;   format_term(E, Str))
    ;   % '_' プレフィックスがない（Prolog の組込み構造など）は通常整形
        format_term(E, Str)
    ).

% 括弧を付けるべきかどうかを判定
should_add_parens(Op1, P1, Op2, P2, Paren) :-
    % 優先度が異なる場合は高い方に括弧を付ける
    P1 \= P2,
    (P1 > P2 ; Paren, Op1 = Op2).
should_add_parens(Op1, P1, Op2, P2, Paren) :-
    % 優先度が同じ場合は結合性を考慮
    P1 = P2,
    Op1 = Op2,
    % 右結合の場合は左側（Paren = false）に括弧を付ける
    % 左結合の場合は右側（Paren = true）に括弧を付ける
    ((is_right_associative(Op1), \+ Paren) ; (\+ is_right_associative(Op1), Paren)).

% 演算子が右結合かどうかを判定
is_right_associative(Op) :-
    ops(a(Op), Prec, following, [PrecRight|_]),
    PrecRight > Prec.
is_right_associative(Op) :-
    ops(a(Op), Prec, leading, [Op|Ns]),
    Ns = [PrecRight|_],
    number(PrecRight),
    PrecRight =:= Prec + 1.


    
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
                debug_print('Evaluating: ', [Goal]),
                clause(Goal, Body),
                eval(Body),
                debug_print('Evaluated: ', [Goal]),
                assertz(eval_cache(Goal, true))
            ),
            % 深さをデクリメント
            nb_setval(eval_depth, Depth)
        ).

% デバッグ出力用のヘルパー述語
debug_print(Label, List) :-
    (nb_getval(debug_mode, true) ->
        % 評価の深さを取得
        nb_getval(eval_depth, Depth),
        % インデントを生成
        generate_indent(Depth, Indent),
        % ラベルとインデントを結合して表示
        atomic_list_concat([Indent, Label], '', IndentedLabel),
        writeln(IndentedLabel),
        % 各項目にもインデントを適用
        maplist(format_debug_item_with_indent(Depth), List)
    ;
        true
    ).

% Rules表示専用のデバッグ出力述語
debug_print_rules(Label, Rules) :-
    (nb_getval(debug_mode, true) ->
        writeln(Label),
        maplist(display_tree, Rules)
    ;
        true
    ).

% インデントを生成する述語
generate_indent(Depth, Indent) :-
    Depth > 0,
    IndentSize is Depth * 2,  % 深さ1につき2スペース
    generate_spaces(IndentSize, Indent).
generate_indent(0, '').

% 指定された数のスペースを生成
generate_spaces(0, '').
generate_spaces(N, Spaces) :-
    N > 0,
    N1 is N - 1,
    generate_spaces(N1, RestSpaces),
    atomic_list_concat([' ', RestSpaces], '', Spaces).

% インデント付きで項目をformat_termで表示
format_debug_item_with_indent(Depth, Item) :-
    generate_indent(Depth, Indent),
    (catch(format_term(Item, Formatted), _, fail) ->
        atomic_list_concat([Indent, Formatted], '', IndentedFormatted),
        write(IndentedFormatted), nl
    ;
        % format_termが失敗した場合は正規形式で表示
        atomic_list_concat([Indent], '', IndentedPrefix),
        write(IndentedPrefix), write_canonical(Item), nl
    ).

% デバッグ用：項目をformat_termで表示（後方互換性のため残す）
format_debug_item(Item) :-
    (catch(format_term(Item, Formatted), _, fail) ->
        write(Formatted), nl
    ;
        % format_termが失敗した場合は正規形式で表示
        write_canonical(Item), nl
    ).

% デバッグ用：項目を正規形式で表示（後方互換性のため残す）
write_canonical_item(Item) :-
    write_canonical(Item), nl.

% 項を横方向の木で表示する述語
display_tree(Term) :-
    display_tree(Term, 0).

display_tree(Term, Indent) :-
    % インデントを出力
    print_indent(Indent),
    % 項の種類を判定
    (compound(Term) ->
        % 複合項の場合
        Term =.. [Functor|Args],
        write(Functor),
        nl,
        % 引数を再帰的に表示
        display_args(Args, Indent + 4)
    ;
        % アトムの場合
        write(Term),
        nl
    ).

% 引数リストを表示
display_args([], _).
display_args([Arg|Rest], Indent) :-
    display_tree(Arg, Indent),
    display_args(Rest, Indent).

% インデントを出力
print_indent(0).
print_indent(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    print_indent(N1).    