[50, ++, 51] >> a(++, 50).
[60, **, 61] >> a(**, 60).
[if, 30, then, 30, else, 31] << a(if, 30).

str(A, A) :- atom(A); number(A).
str(E, Str) :-
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    (As >> a(Op, _); As << a(Op, _)),
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
    (_ >> a(Op2, P2); _ << a(Op2, P2)),
    (_ >> a(Op1, P1); _ << a(Op1, P1)),
    (P1 > P2 -> str(E, Str1), atomic_list_concat(['(', Str1, ')'], '', Str)
            ; str(E, Str)).

tests :-
    str( ++(1, 2), '1 ++ 2' ),
    str( ++(**(1, 2), 3), '1 ** 2 ++ 3' ),
    str( if(1, 2, 3), 'if 1 then 2 else 3' ),
    str( if(1, 2, ++(3, 4)), 'if 1 then 2 else 3 ++ 4' ).