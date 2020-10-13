vyhodnot(ano, ano).
vyhodnot(ne, ne).


vyhodnot(X, ano) :- X = neg(ne).
vyhodnot(neg(ano), ne).


vyhodnot(a(X, Y), V) :-
    vyhodnot(X, HodnotaX), vyhodnot(Y, HodnotaY),
       (
           (HodnotaX = ano, HodnotaY = ano, V = ano);
           (HodnotaX = ano, HodnotaY = ne, V = ne);
           (HodnotaX = ne, HodnotaY = ano, V = ne);
           (HodnotaX = ne, HodnotaY = ne, V = ne)
       ).
vyhodnot(nebo(X, Y), V) :-
    vyhodnot(X, HodnotaX), vyhodnot(Y, HodnotaY),
       (
           (HodnotaX = ano, HodnotaY = ano, V = ano);
           (HodnotaX = ano, HodnotaY = ne, V = ano);
           (HodnotaX = ne, HodnotaY = ano, V = ano);
           (HodnotaX = ne, HodnotaY = ne, V = ne)
       ).
