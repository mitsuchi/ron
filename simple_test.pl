#!/usr/bin/env swipl
% シンプルなregression test

test_file(FileName, ExpectedOutput) :-
    format('Testing ~w... ', [FileName]),
    format(atom(Command), 'swipl ron.pl ~w 2>&1', [FileName]),
    catch(
        (
            process_create('/bin/sh', ['-c', Command], 
                         [stdout(pipe(Out)), stderr(pipe(Err))]),
            read_string(Out, _, Output),
            read_string(Err, _, ErrOutput),
            close(Out),
            close(Err),
            (sub_atom(Output, _, _, _, ExpectedOutput) ->
                writeln('PASS')
            ;
                (writeln('FAIL'),
                 format('Expected to contain: ~w~n', [ExpectedOutput]),
                 format('Got: ~w~n', [Output]),
                 format('Stderr: ~w~n', [ErrOutput]))
            )
        ),
        Error,
        (format('ERROR: ~w~n', [Error]))
    ).

test_file_exists(FileName) :-
    format('Testing ~w (parse only)... ', [FileName]),
    format(atom(Command), 'swipl ron.pl ~w > /dev/null 2>&1', [FileName]),
    catch(
        (
            shell(Command, Status),
            (Status = 0 ->
                writeln('PASS')
            ;
                writeln('FAIL')
            )
        ),
        Error,
        (format('ERROR: ~w~n', [Error]))
    ).

main :-
    writeln('Running regression tests...'),
    nl,
    % example/ディレクトリの.ronファイルで.expectedがあるもののみテスト
    expand_file_name('example/*.ron', AllFiles),
    sort(AllFiles, SortedFiles),
    include(has_expected_file, SortedFiles, FilesWithExpected),
    test_all_files(FilesWithExpected),
    nl,
    length(FilesWithExpected, Count),
    format('Total files tested: ~w~n', [Count]),
    writeln('Regression tests completed.').

% .expectedファイルが存在するかチェック
has_expected_file(FileName) :-
    atom_concat(FileName, '.expected', ExpectedFile),
    exists_file(ExpectedFile).

% ファイルリストを順次テスト
test_all_files([]).
test_all_files([File|Files]) :-
    atom_concat(File, '.expected', ExpectedFile),
    read_file_to_string(ExpectedFile, Expected, []),
    string_concat(Expected, '', ExpectedTrimmed),
    test_file(File, ExpectedTrimmed),
    test_all_files(Files).

:- initialization(main).
