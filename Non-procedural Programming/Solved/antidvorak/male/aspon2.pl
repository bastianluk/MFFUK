?- consult('../functional.pl').
?- consult('../mnozinove_operace.pl').

% Na vstupu máme graf reprezentovaný jako graf(Vrcholy, Hrany) a cislo n a mame urcit, jestli v grafu existuje cyklus delky alespon n. Pokud ano, vratit ho, pokud ne, fail.

aspon2(P,V) :- aspon(2,P,V).

aspon(N,P,V) :- length(P,M), M>=N,!, presne(N,P,V1), N1 is N+1, aspon(N1,P,V2), append(V1,V2,V). 
aspon(_,_,[]).

presne(N,[K-P|Ps],[[K-R|V1]]) :- length([K-P|Ps],M), M=N, !,switch2(P,R), N1 is N-1, presne(N1,Ps,[V1]).
presne(N,[K-P|Ps],V) :- N>0,switch2(P,R), N1 is N-1, presne(N1,Ps,V1), pridej(K-R,V1,Vr1),
    presne(N,Ps,V2), pridej(K-P,V2,Vr2), append(Vr1,Vr2,V).
presne(0,X,[X]).

switch2(true,false).
switch2(false,true).

pridej(R,[S|Ss],[[R|S]|Xs]):-pridej(R,Ss,Xs).
pridej(_,[],[]).


%http://forum.matfyz.info/viewtopic.php?f=169&t=11380

lisi_alespon2(O1, O2) :-
    length(O1, L),
    intersection(O1,O2, O),
    length(O, K),
    L - K >= 2.

aspon2(V, X) :-
    map(fst, V, Promenne),
    length(Promenne, L),
    ohodnoceni(L, VsechnaOhodnoceni),
    foldl(p(Promenne, V),VsechnaOhodnoceni, [], X).

  %  mnohem lepsi reseni, ktere je bohuzel zakazane
  %  findall(PO, ( member(O, VsechnaOhodnoceni), zip(Promenne, O, PO), lisi_alespon2(PO, V) ), X).

ohodnoceni(N, O) :- 
    N > 0,
    M is N-1,
    ohodnoceni(M, P),
    map(pushFront(false), P, R),
    map(pushFront(true), P, S),
    append(R,S,O).

ohodnoceni(0,[[]]).

p(Promenne, V, Ohodnoceni, Acc, [O | Acc]) :-
    zip(Promenne, Ohodnoceni, O),
    lisi_alespon2(O, V), !.

p(_,_,_,Acc,Acc).
