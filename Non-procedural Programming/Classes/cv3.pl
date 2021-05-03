% pridejz(X, S, S2) :- S2 vznikne přidáním X na začátek seznamu S
pridejz(X, S, [X|S]).

% member(X, Seznam) je pravdivý, když Seznam obsahuje prvek X
member(X, [X|S]).
member(X,[A|S]) :- X \= A, member(X, S).

% zip sloučí seznamy
zip([],[],[]).
zip([X|Xs], [Y|Ys], [X,Y|Zs]) :- zip(Xs, Ys, Zs).

% acka_becka je splněný pro seznam áček a stejně dlouhý seznam bček
acka_becka([], []).
acka_becka([a|X], [b|Y]) :- acka_becka(X, Y).

% prefix(P, Seznam) je splěný, pokud P je prefix Seznamu
% dotaz ?- prefix(P, [1,2,3,4]) dosadí za P všechny prefixy seznamu [1,2,3,4]
prefix([], _).
prefix([X|Xs], [X|Ys]) :- prefix(Xs,Ys).

% append(Sez1, Sez2, Sez1a2) připojí Sez1 a Sez2 za sebe
append([],X,X).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).

% ještě jsem zadával přidejNaKonec(X, Sez, SezSXNaKonci)
% testovací dotaz ?- přidejNaKonec(3, [a, b], [a, b, 3]).
% a taky vymaž(X, Seznam, SeznamBezX)
% testovací dotaz ?- vymaž(3, [1,3,2], [1,2]).
% je zde více variant:
%   - vymaž jeden výskyt ?- vymaž(3, [1,3,2,3], [1,2,3]). true
%   - vymaž všechny výskyty ?- vymaž(3, [1,3,2,3], X), X = [1,2]

% prostredni(X, [1,2,3]) vybere prostřední prvek seznamu
% X = 2
prostredni(X, [X]).
prostredni(A,[_|XS]) :- append(YS,[_],XS), prostredni(A,YS).

% ?- otoč([3,2,1], X)
% X = [1,2,3]
otoc([],[]).
otoc([X|Tx],Res) :-
    otoc(Tx, OtocenyTx),
    append(OtocenyTx, [X], Res).

% a vysvětlovali jsme otoč s akumulátorem
% otoc(X, Ak, OtoceneX)
otoc([], OtoceneX, OtoceneX).
otoc([H|T], Ak, OtoceneX) :- otoc(T, [H|Ak], OtoceneX).

% byl i zajímavý dotaz, proč by to nešlo podobně s akumulátorem, ale bez speciální výstupní proměnné. Zkusme to tedy:
otoc2([H|T], Ak) :- otoc2(T, [H|Ak]). % pravidlo, které odlepuje první prvek vstupního pole a přidá ho do akumulátoru.
otoc2([], _). % skončíme, jakmile odlepíme všechny prvky ze vstupu

% takto naimplementovaný otoč skutečně pomocí rekurze buduje otočený seznam, na dotaz `?- otoc2([1,2,3], []).` postupně volá:
% otoc2([2,3], [1]), pak otoc2([3], [2,1]) a pak otoc2([], [3,2,1]), což lze splnit pomocí první klauzule.
% ve výsledku nám tedy na dotaz ?- otoc2([1,2,3], []). napíše true. Takže seznam [1,2,3] lze otočit, ale z Prologu
% nedostaneme konkrétnější odpověď. Srovnejte, že u předchozí implementace vlastně jen přidáváme výstupní proměnnou OtoceneX, abychom
% tuto odpověď vytáhli z hlubin rekurze.
