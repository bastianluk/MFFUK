nahradJeden(X, Y, [X|T], [Y|T]).
nahradJeden(X, Y, [H|T1], [H|T2]) :- nahradJeden(X, Y, T1, T2).


rozdil([], _, []).
rozdil([H1|T1], T2 , T3) :- member(H1, T2), !, rozdil(T1, T2, T3).
rozdil([H1|T1], T2 , [H1|T3]) :- rozdil(T1, T2, T3).
