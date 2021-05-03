pridejz(X, S, [X|S]).


% member(X, [X|S]).
% member(X, [A|S]) :- S \== A, member(X, S).


zip([],[],[]).
zip((X|Xs), (Y|Ys), (X, Y|Zs)) :- zip(Xs, Ys, Zs).

acka_bcka([],[]).
acka_bcka([a|X], [b|Y]) :- acka_bcka(X, Y).

pref([], _).
pref([X|Xs], [X|Ys]) :- pref(Xs,Ys).

append([], X, X).
append([A|X],Y,[A|Z]) :- append(X, Y, Z).

prostredni(X, [X]).
prostredni(A, [_|XS]) :- append(YS, [_], XS), prostredni(A, YS).

% prost(X, Sez, Sez)
% x [x|_] []
% X [_|T] [_, _ | T2]
%


otoc([],[]).
otoc([X|Tx], Res) :- otoc(Tx, OtocTx), append([X], OtocTx, Res).


otocAk([], A, A).
otocAk([H|T], A, OtSez) :- otocAk(T, [H|A], [OtSez]).
