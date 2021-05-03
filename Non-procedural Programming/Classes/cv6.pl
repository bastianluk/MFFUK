% NEDETERMINISMUS

% soucetPodmnoziny(+SeznamCisel, +SoucetPodmnoziny, -PodmnozinaCisel).
% ?- soucetPodmnoziny([1,2,4,10], 7, Podmn).
% Podmn = [2, 4]
% Podmn = [6]
% false
soucetPodmnoziny(_, 0, []).
soucetPodmnoziny([H|T], Soucet, [H|PodmnozinaT]) :-
    SoucetPodmnoziny is Soucet-H,
    soucetPodmnoziny(T, SoucetPodmnoziny, PodmnozinaT).
soucetPodmnoziny([_|T], Soucet, PodmnozinaT) :-
    soucetPodmnoziny(T, Soucet, PodmnozinaT).

% TODO: permutace (pomocí nedeterministického select/3), variace, kombinace

% TODO: generování výrazů, mějme seznam čísel, predikát vzorec(Seznam, Vz) bude splněn, pokud v proměnné
% 	    Vz bude výraz sestavený z čísel v seznamu (použijte append/3 a jeho nedeterministickou
% 	    schopnost rozdělovat seznam na levou a pravou část):
% ?- vzorec([1,2,3], 1-(2+3)).
% true.
% ?- vzorec([1,2,3], Vz).
% Vz = 1+2+3
% Vz = 1+(2+3)
% Vz = 1+2-3
% ... (vzorce vypíše třeba v jiném pořadí)

vzorec([H], H).
vzorec(Sez, NazvyFunkci, Vzorec) :- append(L, P, Sez), L\=[], P\=[], vzorec(L, LV), vzorec(P, PV), 
    Vzorec = xor(LV, PV).

% GRAFY

% reprezentace pomocí seznamu hran:
graf([a, b, c, d, e, f], [(a->b),  (b->c),  (c->a),  (c->d), (d->e), (e->f)]).

% rozhraní:
vrchol(V) :-
    graf(Vrcholy, _),
    member(V, Vrcholy).
hrana(V1, V2) :-
    graf(_, Hrany),
    member((V1->V2), Hrany).

% reprezentace pomocí seznamů následníků:
% např: grafSN([1,2,3,4,5],[1->[2,3,4],2->[5],3->[1,2,4],4->[1],5->[2,3])

% TODO navíc: převod mezi těmito reprezentacemi (pomocí bagof, setof)

% dfs v acyklickém grafu

existuje_cesta_DAG(X, Y) :- hrana(X, Y), !.
existuje_cesta_DAG(X, Z) :- hrana(X, Y), existuje_cesta_DAG(Y, Z).

% příklad: ?- cesta(a, c).
% true, true, true, true, ......

% TODO podle slidů: dfs se seznamem navštívených vrcholů
% existuje_cesta(?X, ?Y, +NavstiveneVrcholy)
% příklad: ?- cesta(a, c, [])
% true
% false

existuje_cesta(X, Y, _) :- hrana(X, Y).
existuje_cesta(X, Z, Navstivene) :- 
    hrana(X, Y), \+ member(Y,Navstivene),
    existuje_cesta(Y, Z, [X|Navstivene]).

% TODO: dfs(?X, ?Y, -Cesta, +NavstiveneVrcholy), který vrací nalezenou cestu

% TODO: dfs, které vrací nalezenou cestu

% TODO: trojuhelnik(X, Y, Z) :- pravdivý pokud existuje trojúhelník mezi těmito vrcholy

% TODO: trojuhelniky(SeznamTroj) :- vrátí seznam všech trojúhelníků (můžete použít setof/3, viz přednáška 4)

% TODO: implementujte nulární existuje_cyklus, který bude pravdivý, pokud v grafu existuje cyklus
% TODO: najít všechny cykly z daného vrcholu
% TODO: otestovat spojitost grafu

% TODO: jezdcova procházka pomocí DFS (nejspíš nám pro opravdové procházky nedoběhne, ale to nevadí)
% potřebujeme predikát skok(p(X,Y), p(X2,Y2)), který je splnitelný pro všechny validní skoky z pozice p(X,Y).
% ?- member(X, [0,1,2]), member(Y, [0,1,2]), cestaKT(p(X,Y), p(X,Y), C, [])

validni(p(X,Y)) :- X >= 0, Y >= 0, X =< 5, Y =< 5.
skok(p(X,Y), p(X2,Y2)) :- X2 is X+1, Y2 is Y+2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-1, Y2 is Y+2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X+1, Y2 is Y-2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-1, Y2 is Y-2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X+2, Y2 is Y+1, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-2, Y2 is Y+1, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X+2, Y2 is Y-1, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-2, Y2 is Y-1, validni(p(X2, Y2)).

cesta(X, Y, [X,Y], _) :- skok(X, Y).
cesta(X, Z, [X|CestaYZ], Navstivene) :- 
    skok(X, Y), \+member(Y, Navstivene), cesta(Y, Z, CestaYZ, [X|Navstivene]).


% TODO navíc: jezdcova procházka s heuristikou

% TODO podle slidů: implementujte breadth first search
% TODO navíc: upravte bfs s použitím rozdílových seznamů

% úloha s přeléváním vody:
% akce(+MaxV1, +MaxV2, +Stav, -NovyStav):-NovyStav je Stav, ktery vznikne
% aplikaci jedne akce na Stav, kdyz nadoby maji maximalni objemy MaxV1 a MaxV2
akce(_, _, s(_, Y), s(0, Y)). % vyliti prvni nadoby
akce(_, _, s(X, _), s(X, 0)). % vyliti druhe nadoby
akce(V1, _, s(_, Y), s(V1, Y)). % naplneni prvni nadoby
akce(_, V2, s(X, _), s(X, V2)). % naplneni druhe nadoby
akce(V1, _, s(X, Y), s(X1, Y1)) :-
    X+Y>V1,
    X1=V1,
    Y1 is Y-(V1-X). % preliti druhe nadoby do prvni, nevejde se vse
akce(V1, _, s(X, Y), s(X1, Y1)) :-
    X+Y=<V1,
    X1 is X+Y,
    Y1=0. % preliti druhe do prvni, vse se vejde 
akce(_, V2, s(X, Y), s(X1, Y1)) :-
    X+Y>V2,
    Y1=V2,
    X1 is X-(V2-Y). % preliti prvni do druhe, nevejde se vse
akce(_, V2, s(X, Y), s(X1, Y1)) :-
    X+Y=<V2,
    Y1 is X+Y,
    X1=0. % preliti prvni do druhe, vejde se vse

% z přednášky:
bfs(Akce, Start, Cil, Cesta) :-
    bfs1(Akce, [[Start]], Cil, CestaRev),
    reverse(CestaRev, Cesta).
bfs1(_, [Xs|_], Cil, Xs) :-
    Xs=[Cil|_].
bfs1(Akce, [[X|Xs]|Xss], Cil, CestaR, Nav) :-
    findall([Y, X|Xs],
            ( call(Akce, X, Y),
              \+ member(Y, [X|Xs])
            ),
            NoveCesty),
    append(Xss, NoveCesty, NovaFronta),
    !,
    bfs1(Akce, NovaFronta, Cil, CestaR, [X|Nav]).

% TODO: zabudujte predikát akce/4 do bfs a najděte řešení

% TODO: Loydova 8 (do šířky)
% TODO: Loydova 15 (s heuristikou)

