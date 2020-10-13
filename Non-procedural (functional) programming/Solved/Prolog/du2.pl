flip3inner([X1, X2, X3 | Xs], [Y1, Y2, Y3 | Xs]) :-
    flip(X1, Y1),
    flip(X2, Y2),
    flip(X3, Y3).
flip3inner([X | Xs], [X | Ys]) :- flip3inner(Xs, Ys).


flip3outter([X1, X2 | Xs], [Y1, Y2 | Ys]) :-
    flip(X1, Y1),
    (
        flipLast2([X2 | Xs], [Y2 | Ys]);
        (
            flip(X2, Y2),
            flipLast(Xs, Ys)
        )
    ).

flip3(Xs, Ys) :- flip3inner(Xs, Ys); flip3outter(Xs, Ys).

flipLast2([X1, X2], [Y1, Y2]) :- flip(X1, Y1), flipLast([X2], [Y2]), !.
flipLast2([X, X2 | Xs], [X, Y2 | Ys]) :- Xs \= [], flipLast2([X2 | Xs], [Y2 | Ys]).

flipLast([X], [Y]) :- flip(X, Y), !.
flipLast([X | Xs], [X | Ys]) :- Xs \= [], flipLast(Xs, Ys).

flip(X, Y) :- X == 1, !, Y = 0.
flip(X, Y) :- X == 0, !, Y = 1.


% Extra
hrana(Xs, Ys) :- flip3(Xs, Ys).


% bfs(+Start,+Cil,-Cesta):- Cesta z
% vrcholu Start do vrcholu Cil
% nalezen? pr?chodem do ???ky.
bfs(Start,Cil,Cesta):-
    bfs1([[Start]], Cil, CestaRev, []),
    reverse(CestaRev,Cesta).

% bfs1(Fronta,Cil,CestaRev,Nav)
bfs1([Xs|_], Cil, Xs, _):- Xs=[Cil|_], !.
bfs1([[X|Xs]|Xss], Cil, CestaR, Nav):-
    findall(
        [Y,X|Xs],
        (
            hrana(X,Y),
            \+member(Y, Nav),
            \+member(Y,[X|Xs])
        ),
        NoveCesty
    ),
    append(Xss, NoveCesty, NovaFronta),
    !,
    bfs1(NovaFronta, Cil, CestaR, [X|Nav]).

build([]).
build([H|T]):- H is 0, build(T).
toZeros(Xs, Dest) :- length(Xs, Len), length(Dest, Len), build(Dest).
flipBits(Xs, Solution) :- toZeros(Xs, Dest), bfs(Xs, Dest, Solution).