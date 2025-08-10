#!/usr/bin/env swipl
% テストランナー - regression testを一括実行
:- consult('test.pl').

% メインエントリーポイント
main :-
    run_all_tests,
    test_stats(failed, Failed),
    (Failed = 0 -> 
        (writeln('All regression tests passed!'), halt(0))
    ; 
        (writeln('Some tests failed!'), halt(1))
    ).

% 引数なしで実行
:- initialization(main).