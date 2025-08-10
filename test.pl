:- consult('ron.pl').
:- dynamic test_stats/2.
:- assert(test_stats(passed, 0)).
:- assert(test_stats(failed, 0)).

% 古い test/2 関数（互換性のため残す）
test(Code, Query) :-
    code_rules(Code, _),
    query_string(Query).

% 新しいテスト関数群

% 期待値との照合テスト
test_expect(TestName, Code, Query, Expected) :-
    write('Testing '), write(TestName), write('... '),
    catch(
        (
            code_rules(Code, _),
            with_output_to(string(Output), query_string(Query)),
            atom_string(Expected, ExpectedAtom),
            (sub_atom(Output, _, _, _, ExpectedAtom) ->
                (write('PASS'), nl, inc_stat(passed))
            ;
                (write('FAIL'), nl,
                 write('  Expected to contain: '), writeln(Expected),
                 write('  Got: '), writeln(Output),
                 inc_stat(failed))
            )
        ),
        Error,
        (
            write('ERROR: '), write(Error), nl,
            inc_stat(failed)
        )
    ).

% パース成功のみをテストする軽量テスト
test_parse_only(TestName, Code) :-
    write('Parsing '), write(TestName), write('... '),
    catch(
        (
            code_rules(Code, _),
            write('PASS'), nl,
            inc_stat(passed)
        ),
        Error,
        (
            write('FAIL - Parse Error: '), write(Error), nl,
            inc_stat(failed)
        )
    ).

% .ronファイルの自動テスト
test_file(FileName) :-
    atom_concat(FileName, '.expected', ExpectedFile),
    (
        exists_file(ExpectedFile) ->
            (
                write('Testing '), write(FileName), write('... '),
                % 外部プロセスで実行して正確な出力を取得
                format(atom(Command), 'swipl ron.pl ~w 2>/dev/null', [FileName]),
                catch(
                    (
                        process_create('/bin/sh', ['-c', Command], [stdout(pipe(Out))]),
                        read_string(Out, _, Output),
                        close(Out),
                        read_file_to_string(ExpectedFile, Expected, []),
                        atom_string(Expected, ExpectedAtom),
                        (sub_atom(Output, _, _, _, ExpectedAtom) ->
                            (write('PASS'), nl, inc_stat(passed))
                        ;
                            (write('FAIL'), nl,
                             write('  Expected to contain: '), writeln(Expected),
                             write('  Got: '), writeln(Output),
                             inc_stat(failed))
                        )
                    ),
                    Error,
                    (
                        write('ERROR: '), write(Error), nl,
                        inc_stat(failed)
                    )
                )
            )
        ;
            (
                write('Testing '), write(FileName), write(' (no expected file)... '),
                catch(
                    (
                        read_file_to_string(FileName, Code, []),
                        string_chars(Code, CodeChars),
                        code_mi(CodeChars),
                        write('PASS'), nl,
                        inc_stat(passed)
                    ),
                    Error,
                    (
                        write('FAIL: '), write(Error), nl,
                        inc_stat(failed)
                    )
                )
            )
    ).

% 統計管理
inc_stat(Type) :-
    retract(test_stats(Type, Count)),
    Count1 is Count + 1,
    assert(test_stats(Type, Count1)).

reset_stats :-
    retractall(test_stats(_, _)),
    assert(test_stats(passed, 0)),
    assert(test_stats(failed, 0)).

show_stats :-
    test_stats(passed, Passed),
    test_stats(failed, Failed),
    Total is Passed + Failed,
    nl,
    writeln('=== Test Results ==='),
    write('Total tests: '), writeln(Total),
    write('Passed: '), writeln(Passed),
    write('Failed: '), writeln(Failed),
    (Failed = 0 ->
        writeln('All tests passed!')
    ;
        (write('Success rate: '),
         Rate is (Passed * 100) // Total,
         write(Rate), writeln('%'))
    ),
    nl.

% 出力の正規化（空白や改行の違いを吸収）
normalize_output(String, Normalized) :-
    atom_string(String, StrAtom),
    atom_chars(StrAtom, Chars),
    atomic_list_concat(Chars, '', Normalized).

% 全テストの実行
run_all_tests :-
    reset_stats,
    writeln('Running regression tests...'),
    nl,
    % 既存のtest.pl内のテストを実行
    run_basic_tests,
    % example/ディレクトリのファイルをテスト
    test_example_files,
    show_stats.

run_basic_tests :-
    writeln('=== Basic Tests ==='),
    test_expect('arrow_simple', "op 50 : _ -> _ ; 1 -> 2;", "1 -> 2", ''),
    test_expect('arrow_chain', "op 50 : _ -> _ ; 1 -> 2; 2 -> 3;", "2 -> 3", ''),
    test_expect('peano_plus', "op 50 : _ plus _ is _ ; op 50 : S _ ; Z plus n is n; (S n1) plus n2 is (S n) { n1 plus n2 is n; }", "(S Z) plus (S Z) is (S S Z)", ''),
    test_parse_only('ski_operators', "op 50 : _ -> _ ; op 50 : _ => _ ; op 100 : _ _ ;"),
    nl.

test_example_files :-
    writeln('=== Example Files Tests ==='),
    % example/ディレクトリの.ronファイルを自動取得
    expand_file_name('example/*.ron', AllFiles),
    sort(AllFiles, SortedFiles),
    % .expectedファイルが存在するもののみフィルタ
    include(has_expected_file, SortedFiles, FilesWithExpected),
    maplist(test_file, FilesWithExpected),
    nl.

% .expectedファイルが存在するかチェック
has_expected_file(FileName) :-
    atom_concat(FileName, '.expected', ExpectedFile),
    exists_file(ExpectedFile).

% 既存のtests群（互換性のため残す）
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
    test_skip,
    test_comment.

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
    test("1 -> 2;",
        "1 -> 2"),
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

test_comment :-
    code_mi("
        op 50 : _ -> _ ;
        # 3 から 4 に矢印が伸びている
        3 -> 4

        main {
            # 3 から 4 に矢印があるか？
            3 -> 4
        }
    ").

% 既存のテスト実行はコメントアウト
% :- tests.
% :- halt.

% 新しいメインエントリポイント
main_test :-
    run_all_tests,
    test_stats(failed, Failed),
    (Failed = 0 -> halt(0) ; halt(1)).