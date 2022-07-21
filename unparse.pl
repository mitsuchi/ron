[50, ++, 51] ^ a(++).
[60, **, 61] ^ a(**).

unparse(Term, ResultAtoms) :-
    functor(Term, _, _, compound),
    Term =.. [Op | Terms],
    unparse(Op, Terms, ResultAtoms)
    ; ResultAtoms = Term.

% op(arg1, arg2, ...) の形の AST を 文字列に変換する
unparse(Op, Terms, ResultAtoms) :-
    As ^ a(Op),      % 演算子表から探す
    numToTerm(As, Terms, ResultAtoms).

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