nadruhou(X, Y):- Y is X*X.

sudy(X) :- 0 is X mod 2.

jeSudyPole([]).
jeSudyPole([X|Xs]) :- sudy(X), jeSudyPole(Xs).



mocnina(_, 0, 1) :- !.
mocnina(X, N, Res) :- NewN is N - 1, mocnina(X, NewN, Temp), Res is Temp*X.


max(X, Y, Z) :- Z is X, Y =< X, !.
max(X, Y, Z) :- Z is Y, Y > X, !.

delka([], 0).
delka([_|Xs], Y) :- delka(Xs, NewY), Y is NewY + 1.

maxSez([X], X) :- !.
maxSez([X| Xs], Max) :- maxSez(Xs, New), max(X, New, Max).


maxSezAk([], Max, Max) :- !.
maxSezAk([X | Xs], Res, Ak) :- max(Ak, X ,NewAk), maxSezAk(Xs, Res, NewAk).
maxSezAk([X|Xs], Max) :- maxSezAk(Xs, Max, X).

nty([X|_], 1, X) :- !.
nty([_|Xs], N, X) :- N >= 1 ,NewN is N - 1, nty(Xs, NewN, X).
