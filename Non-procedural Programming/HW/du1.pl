numeral(0).
numeral(s(X)) :- numeral(X).

lessThan(s(X), s(Y)) :- lessThan(X, Y), X \= Y.
lessThan(0, X) :- numeral(X), X \= 0.

subtract(X , 0, X) :- numeral(X).
subtract(s(X), s(Y), Z) :- subtract(X, Y, Z).

divide(0, _, 0, 0).
divide(X, Y, 0, X) :- lessThan(X, Y).
divide(X, X, s(0), 0).
divide(X, Y, s(R), Rem) :- lessThan(Y, X), subtract(X, Y, Temp), divide(Temp, Y, R, Rem).