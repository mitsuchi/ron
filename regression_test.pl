#!/usr/bin/env swipl
% regression test専用ファイル
% main述語の衝突を避けるため、独立したテストファイル

:- dynamic test_stats/2.
:- assert(test_stats(passed, 0)).
:- assert(test_stats(failed, 0)).

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

% ファイルテスト（外部プロセスで実行）
test_ron_file(FileName) :-
    atom_concat(FileName, '.expected', ExpectedFile),
    write('Testing '), write(FileName), write('... '),
    (
        exists_file(ExpectedFile) ->
            (
                % 外部プロセスでronファイルを実行
                format(atom(Command), 'swipl -g "read_file(~w)." -t halt ron.pl 2>&1', [FileName]),
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
                % 期待値ファイルがない場合は実行成功のみ確認
                format(atom(Command), 'timeout 5 swipl -g "read_file(~w)." -t halt ron.pl > /dev/null 2>&1', [FileName]),
                catch(
                    (
                        shell(Command, Status),
                        (Status = 0 ->
                            (write('PASS (no expected file)'), nl, inc_stat(passed))
                        ;
                            (write('FAIL'), nl, inc_stat(failed))
                        )
                    ),
                    Error,
                    (
                        write('ERROR: '), write(Error), nl,
                        inc_stat(failed)
                    )
                )
            )
    ).

% メインテスト実行
run_regression_tests :-
    reset_stats,
    writeln('Running regression tests...'),
    nl,
    writeln('=== Example Files Tests ==='),
    ExampleFiles = ['example/plus.ron', 'example/ski.ron', 'example/if.ron',
                   'example/arrow1.ron', 'example/arrow2.ron', 'example/arrow3.ron',
                   'example/succ.ron', 'example/type.ron'],
    maplist(test_ron_file, ExampleFiles),
    show_stats.

% メインエントリーポイント
main :-
    run_regression_tests,
    test_stats(failed, Failed),
    (Failed = 0 -> 
        halt(0)
    ; 
        halt(1)
    ).

:- initialization(main).