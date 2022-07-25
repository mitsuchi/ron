ops(++, 50, following, [50, ++, 51]).
ops(**, 60, following, [60, **, 61]).
ops(if, 30, leading, [if, 30, then, 30, else, 31]).

str(A, A) :- atom(A); number(A).
str(E, Str) :-
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    ops(Op, _, _, As),
    str_each(Op, As, Terms, Strs),
    atomic_list_concat(Strs, ' ', Str).

str_each(_, [], _, []).
str_each(Op, [A|As], [Term|Ts], [Str1|Rest]) :-
    number(A),
    str(Term, Op, Str1),
    str_each(Op, As, Ts, Rest).
str_each(Op, [A|As], Ts, [A|Rest]) :-
    not(number(A)),
    str_each(Op, As, Ts, Rest).

str(A, _, A) :- atom(A); number(A).
str(E, Op1, Str) :-
    functor(E, _, _, compound),
    E =.. [Op2 | _],
    ops(Op2, P2, _, _),
    ops(Op1, P1, _, _),
    (P1 > P2 -> str(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
            ; str(E, Str)).

tests :-
    str( ++(1, 2), '1 ++ 2' ),
    str( ++(**(1, 2), 3), '1 ** 2 ++ 3' ),
    str( if(1, 2, 3), 'if 1 then 2 else 3' ),
    str( if(1, 2, ++(3, 4)), 'if 1 then 2 else 3 ++ 4' ).