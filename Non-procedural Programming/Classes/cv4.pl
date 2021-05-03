nadruhou(X, Y) :-  Y is X*X.

even(X) :- 0 is X mod 2.

evenPole([]).
evenPole([X|XS]):-even(X),evenPole(XS).

% mocnina(X, N, Vys)

mocnina(_, 0, 1) :- !.
mocnina(X, 1, X) :- !.
mocnina(X, N, Vys) :-
    N >= 2,
    Nminus1 is N - 1,
    mocnina(X, Nminus1, MensiVys),
    Vys is MensiVys * X.


max(X,Y,X) :- X >= Y, !.
max(X,Y,Y) :- X < Y.

delka([], 0).
delka([_| Xs], Y) :- delka(Xs, NewY), Y is NewY + 1.
maxPole([X],X).
maxPole([Y|Ys], Max) :- maxPole(Ys,Ms), max(Ms,Y,Max).

delkaAk([], Vys, Vys).
delkaAk([_|T], Vys, Ak) :-
    VetsiAk is Ak+1,
    delkaAk(T, Vys, VetsiAk).

delkaAk(Sez, Vys) :- delkaAk(Sez, Vys, 0).

maxAk([], Max, Max).
maxAk([H|T], Max, ProzatimMax) :-
    max(ProzatimMax, H, NovyMax),
    maxAk(T, Max, NovyMax).

maxAk([H|T], Max) :- maxAk(T, Max, H).

fold(_, [], Vys, Vys).
fold(Pred, [H|T], Ak, Vys) :-
    call(Pred, H, Ak, Vys1),
    fold(Pred, T, Vys1, Vys).

plus(X, Y, Z) :- Z is X + Y.

n_prvek([L|Ls], 0, L) :- !.
n_prvek([L|Ls], N, R) :-
    N >= 1,
    Nm1 is N - 1,
    n_prvek(Ls, Nm1, R).
