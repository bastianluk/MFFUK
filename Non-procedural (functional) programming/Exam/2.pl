% Excercise 2 - Bastian Lukas

% a)
% record bude array vzdy o 3 polozkach [jmeno, cas, doba]

% b)
% vstupni seznam bude array dvojic s polozkoku jmeno a seznam zaznamu
% (jmeno, [arrayOfRecords])

% c) inf/4
inf(VstupniSeznam, Cestovatel, Krit, Result) :- najdiKontakty(VstupniSeznam, Cestovatel, Kontakty)
% Najde kontakty pro nejakeho cestovatele - infikovaneho
najdiKontakty([V|StupniSeznam], Cestovatel, Kontakty) :- V is [Jmeno, Records], (
                                                             (
                                                                 Jmeno = Cestovatel,
                                                                 Kontakty is Records
                                                             );
                                                             najdiKontakty(StupniSeznam, Cestovatel, Kontakty)
                                                         )
% Dale bych podle krit hodnoty a hodnot v recordech porovnaval, kdo je infikovany a dale tranzitivne hledal, kdo je nakazeny od nakazenych ale drzet si seznam jiz navstivenych v nejakych casech
    



% d)


% e)
% Vyznam je hlavne ve spotrebe pameti - pokud se rekurze vola az jako posledni tak vim, ze mi staci stejny blok pameti na vypocet a z jeho konce se potom vraci vysledek