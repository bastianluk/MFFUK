% Excercise 1 - Bastian Lukas

% setrid(+Xs,-Ys) :- Ys je seznam přirozených čísel ze seznamu Xs setříděný vzestupně
setrid(Xs,Ys) :- append(A,[H1,H2|B],Xs), H1 > H2, !, append(A,[H2,H1|B],Xs1), setrid(Xs1,Ys).

% a) doplnit za:
setrid(X, X) :- !.

% b) doplnit pred:

% missing

% c)
% Jde o zeleny rez, ktery po usporadani zajisti, ze se znovu nebude brat mensi prefix seznamu nez A a A bude uz setrizene

% d)
% melo by jit o bubble sort - postupne se swapuji prvky, ktere nejsou ve spravnem poradi