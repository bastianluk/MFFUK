% ŘEZY
% materiály: Přednáška 4, LPN kapitola 10

% TODO: nahradJeden(+X, +Y, +Sez, -Vys) :- nahradí jeden výskyt X v Sez za Y.
% nahradJeden(1,x,[4,3,2,1,1,2,1], X).
% X = [4, 3, 2, x, 1, 2, 1]
% X = [4, 3, 2, 1, x, 2, 1]
% X = [4, 3, 2, 1, 1, 2, x]

% TODO: nahradPrvni(X,Y,Sez,Vys) :- nahradí první výskyt X v Sez za Y
% zkuste pomocí řezu a pak pomocí once() z přednášky
% nahradPrvni(1,x,[4,3,2,1,1,2,1], X).
% X = [4, 3, 2, x, 1, 2, 1]

% TODO: rozdil(+Sez1, +Sez2, -Rozd) :- Rozd je seznam prvků ze Sez1, které nejsou v Sez2
% rozdil([1,2,3], [2,3], X).
% X = [1].

% TODO: zplosti(+Sez, -Zplosteny) :- 
%     projde všechny seznamy v Sez a jejich prvky přidá do Zplosteny
% ?- zplosti([1,[1,[],[1],2],3], X).
% X = [1, 1, 1, 2, 3].

% TODO: zplosti naprogramované pomoci if-then-else
% ?- zplosti2([1,[1,[],[1],2],3], X).
% X = [1, 1, 1, 2, 3].

% ROZDÍLOVÉ SEZNAMY
% materiály: Přednáška 4

% TODO podle slidů: ?- na_normalni([1,2,3|S]-S, X).
% S = [],
% X = [1, 2, 3]

% TODO podle slidů: ?- na_rozdilovy([1,2,3], X).
% X = [1, 2, 3|_1504]-_1504

% TODO podle slidů: ?- zretez([1,2,3|S1]-S1, [a, b|S2]-S2, X).
% S1 = [a, b|S2],
% X = [1, 2, 3, a, b|S2]-S2.

% TODO: implementace 0(1) fronty
% enqueue(+X, +RozdilovySeznam, -RozdilovySeznamsXnaKonci)
% ?- enqueue(4, [1,2,3|S]-S, X).
% S = [4|_1538],
% X = [1, 2, 3, 4|_1538]-_1538
% zkuste nejdřív pomocí zretez/3, pak přepište na verzi bez těla klauzule.

% TODO: dequeue(+RozdilovySeznam, -X, RozdilovySeznamBezPrvnihoPrvku)
% dequeue([1,2,3|S]-S, X, Y).
% X = 1,
% Y = [2, 3|S]-S

% STROMY
% materiály: přednáška 3
% 
% mějme strom reprezentovaný strukturou t(LevyPodstrom, HodnotaVrcholu, PravyPodstrom)
% list budeme reprezentovat jako t(nil, HodnotaListu, nil)
% příklad stromu: t(t(nil, 1, nil), 5, t(t(nil, 6, nil), 8, nil))
% TODO podle slidů: in(V, Strom) :- pravdivý pokud Strom obsahuje V.

% TODO: soucetStrom(+Strom, -Vys) :- sečte všechny vrcholy a výsledek uloží do Vys

% GRAFY
% materiály: přednáška 5
% mějme graf reprezentovaný jako graf(Vrcholy, Hrany)
graf([a, b, c, d, e, f], [a->b, b->c, c->a, c->d, e->f]).

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

% TODO: trojuhelnik(X, Y, Z) :- pravdivý pokud existuje trojúhelník mezi těmito vrcholy

% TODO: trojuhelniky(SeznamTroj) :- vrátí seznam všech trojúhelníků (můžete použít setof/3, viz přednáška 4)

% TODO: implementujte nulární existuje_cyklus, který bude pravdivý, pokud v grafu existuje cyklus
% hint: zkuste implementovat upravený dfs2, který bude umět hledat cestu z vrcholu V k sobě samému
% pak spusťe dfs2 pro všechny vrcholy grafu.
% hint2: určitě musíte upravit bázi dfs(X,X,_). tak, aby neskončila triviálně pro prázdný seznam

% TODO podle slidů: implementujte breadth first search
% TODO: upravte bfs s použitím rozdílových seznamů

