rodic(alfred, boris).
rodic(alfred, alex).

muz(alfred).
muz(boris).
muz(alex).

bratr(X, Y) :- rodic(Z, X), rodic(Z, Y), muz(X), X\=Y.


numeral(0).
% Rekurze.
numeral(s(X)) :- numeral(X).

zdvoj(0, 0).
zdvoj(s(X), s(s(Y))) :- zdvoj(X, Y).

sudy(0).
sudy(s(s(X))) :- sudy(X).


sudy2(X) :- zdvoj(_, X).

soucet(0, X, X).
soucet(s(X), Y, s(Z)) :- soucet(X, Y, Z).

soucin(0, _, 0).
soucin(s(X), Y, Z) :- soucet(Temp, Y, Z), soucin(X, Y, Temp).
