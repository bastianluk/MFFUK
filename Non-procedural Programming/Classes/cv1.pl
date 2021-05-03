% VZTAHY MEZI LIDMI

muz(emil).
muz(jirka).
muz(martin).
zena(sebastian).
zena(jolanda).

% testovací dotaz ?- muz(X).
% zobrazí všechny muže, další výsledek středníkem

miluje(sebastian, emil).
miluje(emil, sebastian).
miluje(sebastian, martin).
miluje(jirka, emil).
% miluje(jolanda, emil).
miluje(jolanda, X) :-
    vlasy(X, hnedy),
    X \= emil.

% koho miluje jolanda?
% ?- miluje(jolanda, X)

svatba(X,Y) :- miluje(X,Y), miluje(Y,X). % čárka jako logické and, musí platit obě podmínky

vlasy(emil, hnedy).
vlasy(sebastian, hnedy).


trojuhelnik(X,Y,Z) :- miluje(X,Y), miluje(Y,Z), miluje(Z,X).
% trojsvatba, na kterou byl dotaz:
trojsvatba(X, Y, Z) :- svatba(X, Y), svatba(Y, Z), svatba(Z, X).

% rekurzivní definice milostné cesty
% nejprve definice cesty o délce dva
cesta2(X,Y) :- miluje(X,Z), miluje(Z,Y).
% a nyní obecná cesta
cesta(X,Y) :- cesta2(X,Y). % milostná cesta mezi X a Y existuje pokud mezi X a Y existuje milostná cesta délky 2
cesta(X,Y) :- miluje(X,Prostrednik), cesta(Prostrednik, Y). % a nebo existuje nějaký člověk, kterého miluje X a zároveň od něj již vede cesta k člověku Y.
% upozornění: v milostném trojúhelníku se tenhle predikát zacyklí, jak tomu předejít si ukážeme jindy.

% VYHODNOCOVÁNÍ LOGICKÝCH FORMULÍ

% nejprve jednoduché případy
vyhodnot(ano, ano).
vyhodnot(ne, ne).
% můžeme napsat i pomocí středníku, které funguje jako logické or:
% vyhodnot(X, Y) :- X = ano, Y = ano ; X = ne, Y = ne.

% negace
% vyhodnot(X, ano) :- X = neg(ne).
% vyhodnot(X, ne) :- X = neg(ano).
% případně:
% vyhodnot(neg(ne), ano).
% vyhodnot(neg(ano), ne).
% !!!ale pozor!!! - taková implementace nefunguje pro dotaz ?- vyhodnot(neg(neg(ano)), ano). který by měl být správně splněný!
% protože jsme zadali pravidla pouze pro neg(ano) a neg(ne) a nikoli pro neg(neg(ano)).
% opravená implementace:
vyhodnot(neg(X), ne) :- vyhodnot(X, ano). % tedy vyhodnot(neg(X), ne) platí, pokud platí vyhodnot(X, ano).
vyhodnot(neg(X), ano) :- vyhodnot(X, ne).
% nyní tedy dotaz ?- vyhodnot(neg(neg(ano)), ano).  (tento matchuje s klauzulí predikátu vyhodnot/2 definovanou na 52. řádku)
% spustí dotaz ?- vyhodnot(neg(ano), ne).           (tento matchuje s klauzulí predikátu vyhodnot/2 definovanou na 51. řádku)
% a ten spustí dotaz ?- vyhodnot(ano, ano).         (tento matchuje s klauzulí predikátu vyhodnot/2 definovanou na 39. řádku)
% který lze splnit a díky tomu jde splnit i všechny předchozí dotazy

% a zároveň:
vyhodnot(a(X, Y), V) :-
    vyhodnot(X, HodnotaX), % proměnnou HodnotaX se prolog pokusí zunifikovat s ano nebo s ne pomocí všech pravidel vyhodnot/2
    vyhodnot(Y, HodnotaY),
    ( % zároveň musí platit některá z těchto kombinací vstupů:
        (HodnotaX = ano, HodnotaY = ano, V=ano); % středník jako logické nebo
        (HodnotaX = ano, HodnotaY = ne,  V=ne);
        (HodnotaX = ne,  HodnotaY = ano, V=ne);
        (HodnotaX = ne,  HodnotaY = ne,  V=ne)
    ).

% lze napsat i kratší varianta:
% vyhodnot(a(X, Y), V) :-
%     vyhodnot(X, HX), % proměnnou HodnotaX se prolog pokusí zunifikovat s ano nebo s ne pomocí všech pravidel vyhodnot/2
%     vyhodnot(Y, HY),
%     ( % zároveň musí platit některá z těchto kombinací vstupů:
%         (HX = ano, HY = ano, V=ano); % buď jsou HX a HY ano, pak V je ano
%         ((HX = ne; HY = ne),  V=ne) % nebo je HX nebo HY ne, pak V je ne
%     ).

% implementace vyhodnocení logického and z dotazu kolegy:
% vyhodnot(a(X, Y), V) :-
%     vyhodnot(X, HodnotaX), % vyhodnotíme si 
%     vyhodnot(Y, HodnotaY),
%     (   HodnotaX=ano,
%         HodnotaY=ano,
%         V=ano
%     ;   V=ne
%     ).
% logika kolegy byla: buď splníme HodnotaX = ano, HodnotaY = ano a pak musí být V = ne. Jinak V = ne
% pozor!!! Větev V=ne nemá žádné další podmínky, takže lze splnit při libovolné kombinaci HodnotaX a HodnotaY
% konkrétně by byl splněn třeba i dotaz ?- vyhodnot(a(ano, ano), ne), protože V=ne nemá žádné další podmínky

% implementace logického nebo:
vyhodnot(nebo(X, Y), V) :-
    vyhodnot(X, HX),
    vyhodnot(Y, HY),
    (   HX=ano,
        V=ano
    ;   HY=ano,
        V=ano
    ;   HX=ne,
        HY=ne,
        V=ne
    ).

% příklad dotazu:
% ?- vyhodnot(a(nebo(X, ne), nebo(Y, ano)), ano).
% neboli jak musíme nastavit proměnné X a Y, aby celá formule byla vyhodnocená jako ano?
% všimněte si na výstupu tautologických zápisů, jak se jich zbavit zjistíme jindy