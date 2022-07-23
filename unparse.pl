[50, ++, 51] ^ a(++).
[60, **, 61] ^ a(**).

str(A, A) :- atom(A); number(A).
str(E, Str) :-
    functor(E, _, _, compound),
    E =.. [Op | Terms],
    As ^ a(Op),
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
    [A2|_] ^ a(Op2),
    [A1|_] ^ a(Op1),
    (A1 > A2 -> str(E, Str1), atomic_list_concat(['(', Str1, ')'], ' ', Str)
            ; str(E, Str)).
