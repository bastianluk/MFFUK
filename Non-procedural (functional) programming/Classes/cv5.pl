% ŘEZY
% materiály: Přednáška 4, LPN kapitola 10

% TODO: nahradJeden(+X, +Y, +Sez, -Vys) :- nahradí jeden výskyt X v Sez za Y.
% nahradJeden(1,x,[4,3,2,1,1,2,1], X).
% X = [4, 3, 2, x, 1, 2, 1]
% X = [4, 3, 2, 1, x, 2, 1]
% X = [4, 3, 2, 1, 1, 2, x]

nahradJeden(X, Y, [X|T], [Y|T]).
nahradJeden(X, Y, [H|T1], [H|T2]) :-
    nahradJeden(X, Y, T1, T2).

% TODO: nahradPrvni(X,Y,Sez,Vys) :- nahradí první výskyt X v Sez za Y
% zkuste pomocí řezu a pak pomocí once() z přednášky
% nahradPrvni(1,x,[4,3,2,1,1,2,1], X).
% X = [4, 3, 2, x, 1, 2, 1]

nahradPrvni(X, Y, [X|T], [Y|T]).
nahradPrvni(X, Y, [H|T1], [H|T2]) :-
    once(nahradPrvni(X, Y, T1, T2)).

% TODO: rozdil(+Sez1, +Sez2, -Rozd) :- Rozd je seznam prvků ze Sez1, které nejsou v Sez2
% rozdil([1,2,3], [2,3], X).
% X = [1].

rozdil([], _, []).
rozdil([H|T], Y, Vys) :-
    member(H, Y),
    !,
    rozdil(T, Y, Vys).
rozdil([H|T], Y, [H|Vys]) :-
    rozdil(T, Y, Vys).

% TODO: zplosti(+Sez, -Zplosteny) :- 
%     projde všechny seznamy v Sez a jejich prvky přidá do Zplosteny
% ?- zplosti([1,[1,[],[1],2],3], X).
% X = [1, 1, 1, 2, 3].

zplosti([], []).
zplosti([H|T], Vys) :-
    is_list(H),
    !,
    zplosti(H, HZpl),
    zplosti(T, TZpl),
    append(HZpl, TZpl, Vys).
zplosti([H|T], [H|TZpl]) :-
    zplosti(T, TZpl).

% TODO: zplosti naprogramované pomoci if-then-else
% ?- zplosti2([1,[1,[],[1],2],3], X).
% X = [1, 1, 1, 2, 3].

zplosti2([], []).
zplosti2([H|T], Vys) :-
    (   is_list(H)
    ->  zplosti2(H, HZpl),
        zplosti2(T, TZpl),
        append(HZpl, TZpl, Vys)
    ;   Vys=[H|TZpl],
        zplosti2(T, TZpl)
    ).

% ROZDÍLOVÉ SEZNAMY
% materiály: Přednáška 4

% TODO podle slidů: ?- na_normalni([1,2,3|S]-S, X).
% S = [],
% X = [1, 2, 3]
na_normalni(Sez-[], Sez).

% TODO podle slidů: ?- na_rozdilovy([1,2,3], X).
% X = [1, 2, 3|_1504]-_1504
na_rozdilovy([], S-S).
na_rozdilovy([H|T], [H|TRozd]-S) :-
    na_rozdilovy(T, TRozd-S).

% TODO podle slidů: ?- zretez([1,2,3|S1]-S1, [a, b|S2]-S2, X).
% S1 = [a, b|S2],
% X = [1, 2, 3, a, b|S2]-S2.
zretez(A-B, B-C, A-C).

% TODO: implementace 0(1) fronty
% enqueue(+X, +RozdilovySeznam, -RozdilovySeznamsXnaKonci)
% ?- enqueue(4, [1,2,3|S]-S, X).
% S = [4|_1538],
% X = [1, 2, 3, 4|_1538]-_1538
% zkuste nejdřív pomocí zretez/3, pak přepište na verzi bez těla klauzule.
enqueue(X, A-B, A-C) :-
    zretez(A-B,
           [X|S]-S,
           A-C).
enqueue2(X, A-[X|S], A-S).

% TODO: dequeue(+RozdilovySeznam, -X, RozdilovySeznamBezPrvnihoPrvku)
% dequeue([1,2,3|S]-S, X, Y).
% X = 1,
% Y = [2, 3|S]-S
dequeue([H|T]-S, H, T-S).

% STROMY
% materiály: přednáška 3
% 
% mějme strom reprezentovaný strukturou t(LevyPodstrom, HodnotaVrcholu, PravyPodstrom)
% list budeme reprezentovat jako t(nil, HodnotaListu, nil)
% příklad stromu: t(t(nil, 1, nil), 5, t(t(nil, 6, nil), 8, nil))
% TODO podle slidů: in(V, Strom) :- pravdivý pokud Strom obsahuje V.
in(V, t(_, V, _)) :-
    !.
in(V, t(LevyS, _, _)) :-
    in(V, LevyS),
    !.
in(V, t(_, _, PravyS)) :-
    in(V, PravyS).
% poznámka: řešení je pro obecný binární strom, ne pro BVS jako na slidech.

% TODO: soucetStrom(+Strom, -Vys) :- sečte všechny vrcholy a výsledek uloží do Vys

% GRAFY
% materiály: přednáška 5
% mějme graf reprezentovaný jako graf(Vrcholy, Hrany)
graf([a, b, c, d, e, f], [(a->b),  (b->c),  (c->a),  (c->d),  (e->f)]).

% rozhraní:
vrchol(V) :- graf(Vrcholy,_), member(V,Vrcholy).
hrana(V1, V2):- graf(_,Hrany), member(V1->V2,Hrany).
% zkuste: ?- hrana(X,Y)
% určitě lze používat i obecnější definici hrana(V1, V2, graf(Vrcholy,Hrany)):- member(h(V1,V2),Hrany).
% která má v argumentu vstupní graf. Pro přehlednost ale zatím můžeme používat toto jednodušší rozhraní pro jeden konkrétní graf.

% TODO podle slidů: dfs(X, Y) :- pravdivý pokud existuje v grafu cesta z vrcholu X do vrcholu Y.
% dfs(a, c)
% true
% dfs(a, f)
% false

dfs(X, Y) :-
    dfs(X, Y, [X]).
dfs(X, X, _).
dfs(X, Z, Navs) :-
    hrana(X, Y),
    \+ member(Y, Navs),
    dfs(Y, Z, [Y|Navs]).

% TODO: trojuhelnik(X, Y, Z) :- pravdivý pokud existuje trojúhelník mezi těmito vrcholy
trojuhelnik(X, Y, Z) :-
    hrana(X, Y),
    hrana(Y, Z),
    hrana(Z, X).

% TODO: trojuhelniky(SeznamTroj) :- vrátí seznam všech trojúhelníků (můžete použít setof/3, viz přednáška 4)
trojuhelniky(Sez) :-
    setof([A, B, C],
          trojuhelnik(A, B, C),
          Sez).

% TODO: implementujte nulární existuje_cyklus, který bude pravdivý, pokud v grafu existuje cyklus
% hint: zkuste implementovat upravený dfs2, který bude umět hledat cestu z vrcholu V k sobě samému
% pak spusťe dfs2 pro všechny vrcholy grafu.
% hint2: určitě musíte upravit bázi dfs(X,X,_). tak, aby neskončila triviálně pro prázdný seznam
dfs2(X, X, [_|_]).
dfs2(X, Z, Navs) :-
    hrana(X, Y),
    \+ member(Y, Navs),
    dfs(Y, Z, [Y|Navs]).

existuje_cyklus :-
    vrchol(V),
    dfs2(V, V, []).

% TODO podle slidů: implementujte breadth first search
% TODO: upravte bfs s použitím rozdílových seznamů
