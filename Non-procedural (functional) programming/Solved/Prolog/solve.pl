word(astante, a,s,t,a,n,t,e).
word(astoria, a,s,t,o,r,i,a).
word(baratto, b,a,r,a,t,t,o).
word(cobalto, c,o,b,a,l,t,o).
word(pistola, p,i,s,t,o,l,a).
word(statale, s,t,a,t,a,l,e).


solvePuzzle(V1, V2, V3, H1, H2, H3) :-
    word(V1, _, V12, _, V14, _, V16, _),
    word(V2, _, V22, _, V24, _, V26, _),
    word(V3, _, V32, _, V34, _, V36, _),
    word(H1, _, H12, _, H14, _, H16, _),
    word(H2, _, H22, _, H24, _, H26, _),
    word(H3, _, H32, _, H34, _, H36, _),
    V12 = H12, V22 = H14, V32 = H16,
    V14 = H22, V24 = H24, V34 = H26,
    V16 = H32, V26 = H34, V36 = H36,
    V1 \= V2,
    V1 \= V3,
    V1 \= H1,
    V1 \= H2,
    V1 \= H3,
    V2 \= V3,
    V2 \= H1,
    V2 \= H2,
    V2 \= H3,
    V3 \= H1,
    V3 \= H2,
    V3 \= H3,
    H1 \= H2,
    H1 \= H3,
    H2 \= H3.




% pøíklad dotazu:
% ?- solvePuzzle(V1, V2, V3, H1, H2, H3).
% V1 = astante,
% V2 = cobalto,
% ...
% (celkem by to mìla být 2 øešení)
