muz(emil).
muz(jirka).
muz(martin).
zena(sebastian).
zena(jolanda).


miluje(jolanda, emil).
miluje(emil, sebastian).
miluje(sebastian, martin).
miluje(sebastian, emil).
miluje(jirka, emil).
miluje(jolanda, X) :- vlasy(X, hnedy).


vlasy(emil, hnedy).
vlasy(sebastian, hnedy).


svatba(X, Y) :- miluje(X, Y), miluje(Y, X).


trojuhelnik(X, Y, Z) :- miluje(X, Y), miluje(Y, Z), miluje(Z, X).



cesta2(X, Y) :- miluje(X, Z), miluje(Z, Y).

cesta(X, Y) :- cesta2(X, Y).
cesta(X, Y) :- miluje(X, Z), cesta2(Z, Y).
