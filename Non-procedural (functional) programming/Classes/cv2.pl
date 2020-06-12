% 
% Poznámky k ovládání SWI-Prologu:
% - konec dotazu `.`. Tečku lze připsat i když omylem odentrujeme bez ní. Tj. tyhle dva dotazy jsou ekvivalentní:

%     ?- numeral(s(0)).
%     true.

%     ?- numeral(s(0))
%     |    .
%     true.

% - přerušení pomocí 2x Control+C, pak se zobrazí: `Action (h for help) ?`. Nejčastěji potřebujeme akci `a` (abort).
% - vypnutí Prologu pomocí Control+D nebo predikátu `halt.`.
% - při vyhodnocování dotazu: další řešení `;` a ukončit enumeraci řešení `.`

rodic(boris, alfred).
rodic(boris, alex).

muz(alfred).
muz(boris).
muz(alex).

bratr(X, Y) :- rodic(Z, X), rodic(Z, Y), X\=Y, muz(X).

% numeral(X) zjistí jestli je X numerál
numeral(0).
numeral(s(X)) :- numeral(X).
% testovací dotazy:
% ?- numeral(s(0)).
% ?- numeral(s(s(0))).

% zdvoj(X, Y) :- Y je dvojnásobek X
zdvoj(0, 0).
zdvoj(s(X), s(s(Y))) :- zdvoj(X, Y).

% sudy(X) :- X je sudý numerál
sudy(0).
sudy(s(s(X))) :- sudy(X).
% alternativní definice predikátu sudy/1
sudy2(X) :- zdvoj(_, X).

% soucet(X, Y, Z) :- X + Y je Z
soucet(0, Y, Y).
soucet(s(MensiX), Y, s(MensiZ)) :- soucet(MensiX, Y, MensiZ).
% příklad dotazu na rozklad čísla na součet dvou jiných: ?- soucet(X,Y,s(s(0))).

% alternativní definice:
% soucet2(0, 0, 0).
% soucet2(X, s(Y), s(Z)) :- soucet2(X, Y, Z).
% soucet2(s(X), Y, s(Z)) :- soucet2(X, Y, Z).
% ta má ale nevýhodu v tom, že hlásí víc různých řešení.

mult(0,_,0).
mult(_,0,0).
mult(s(X), Y, Z) :- soucet(A, Y, Z), mult(X, Y, A).
