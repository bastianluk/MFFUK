-- Internetový obchod s knihami

-- Testovací pøíklady

-- Pro funkènost je potøeba nejprve databázi naplnit demonstraèními daty v oddìleném souboru.
-- Jednotlivé pøíkazy v tomto skriptu pøedpokládají spuštìní všech pøedcházejících nechybových pøíkazù.

-- Testy integritních omezení
-----------------------------

-- Vzor správných knih, která jdou vložit a na kterých budou ukázány integritní omezení
insert into Kniha values (12, 1, 1, 1, 1, 'Název knihy', 'Popis knihy', NULL, 153, '15-54345-212-4', 5, 5, 199.00);
insert into Kniha values (13, 2, 2, 2, 2, 'Název jiné knihy', 'Popis jiné knihy', 2005, NULL, '15-54345-212-5', 2, 3, 299.00);
-- Chyba - u knihy není možné nastavit nesmyslný rok vydání
update Kniha set RokVydani = 45454 where Id = 12
-- Chyba - u knihy není možné nastavit duplicitní ISBN
update Kniha set ISBN = '15-54345-212-5' where Id = 12
-- Chyba - u knihy není možné nastavit neplatné ISBN
update Kniha set ISBN = 'ISBN: 15-54345-212-5' where Id = 12
-- Chyba - cena knihy musí být nezáporná
update Kniha set Cena = -258 where Id = 12
-- Chyba - skladové zásoby musí být nezáporné
update Kniha set PocetNaSkladu = PocetNaSkladu - 50 where Id = 12
-- Chyba - autor s daným Id neexistuje
update Kniha set IdAutor = 5453 where Id = 12

-- Chyba - nejde vložit k jedné knize jinou jako související dvakrát
insert into Souvisejici values (12, 13), (12, 13);

-- Je možné do rùzných hlavních kategorií vložit stejnì nazvané podkategorie
insert into Kategorie values (500, NULL, 'Hlavní 1', NULL),
              (501, NULL, 'Hlavní 2', NULL),
              (502, 500, 'Podkategorie', NULL),
              (503, 501, 'Podkategorie', NULL);
-- Chyba - není možné vložit do jedné hlavní kategorie dvì stejnì pojmenované podkategorie
insert into Kategorie values (504, NULL, 'Hlavní', NULL),
              (505, 504, 'Podkategorie', NULL),
              (506, 504, 'Podkategorie', NULL);

-- Vytvoøení nové objednávky ve stavu košíku
insert into Objednavka values (7, 'Kosik', NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL);
-- Chyba - objednávku ve stavu 'Kosik' nejde zaplatit
update Objednavka set Zaplacena = 1 where Id = 7;
-- Chyba - objednávka v jiném stavu než 'Kosik' musí mít vyplnìné všechny údaje
update Objednavka set Stav = 'Potvrzena' where Id = 7;

-- Pøíklad odeslané objednávky
select * from Objednavka where Id = 2;
-- Chyba - odeslaná objednávka nemùže být nezaplacená
update Objednavka set Zaplacena = 0 where Id = 2;
-- Chyba - objednávka musí mít PSÈ složené pouze z èíslic a délku 5
update Objednavka set Psc = '143 00' where Id = 2;
update Objednavka set Psc = '1430' where Id = 2;

-- Pøíklad - do prázdného košíku je vložena položka
insert into PolozkaObjednavky values (7, 1, 3);
select * from PolozkaObjednavky where IdObjednavka = 7;
-- Objednávka jde smazat, pøestože v sobì má položku
delete from Objednavka where Id = 7;
-- Položka z objednávky byla kaskádovì smazána
select * from PolozkaObjednavky where IdObjednavka = 7;



-- Testy triggerù
-----------------

-- Hlavní kategorie s Id = 1 má podkategorii s Id 7
select * from Kategorie where Id in (1, 7);
-- Vytvoøíme novou kategorii bez nadøazené kategorie
insert into Kategorie values (20, NULL, 'Kat', NULL);
-- Chyba - nejde novou kategorii zaøadit nad dvì kategorie (kategorie jsou max dvouúrovòové)
update Kategorie set IdNadrazenaKategorie = 20 where Id = 1;
-- Chyba - nejde novou kategorii zaøadit pod dvì kategorie
update Kategorie set IdNadrazenaKategorie = 7 where Id = 20;

-- Vytvoøení kategorie s dvìma podkategoriemi
insert into Kategorie values (21, NULL, '21', NULL),
              (22, 21, '22', NULL),
              (23, 21, '23', NULL);
select * from Kategorie where Id in (21, 22, 23);
-- Odstranìní jedné podkategorie
delete from Kategorie where Id = 22;
select * from Kategorie where Id in (21, 22, 23);
-- Odstranìní nadøazené kategorie (je s ní odstranìna automaticky i podkategorie pomocí triggeru)
delete from Kategorie where Id = 21;
select * from Kategorie where Id in (21, 22, 23);

-- Vytvoøení ètyø vzájemnì souvisejících knih
insert into Kniha values (14, 1, 1, 1, 1, '14', 'Popis 14', NULL, 153, '15-54345-212-14', 5, 5, 199.00);
insert into Kniha values (15, 2, 2, 2, 2, '15', 'Popis 15', 2005, NULL, '15-54345-212-15', 2, 3, 299.00);
insert into Kniha values (16, 2, 2, 2, 2, '16', 'Popis 16', 2005, NULL, '15-54345-212-16', 2, 3, 299.00);
insert into Kniha values (17, 2, 2, 2, 2, '17', 'Popis 17', 2005, NULL, '15-54345-212-17', 2, 3, 299.00);
insert into Souvisejici values (14, 15), (15, 14), (16, 14), (14, 16), (16, 15), (15, 16), (14, 17), (15, 17), (16, 17), (17, 14), (17, 15), (17, 16);
select * from Kniha where Id in (14, 15, 16, 17);
select * from Souvisejici where IdKniha in (14, 15, 16, 17);
-- Odstranìní dvou knih (jsou automaticky odstranìny i položky v souvisejících)
delete from Kniha where Id in (14, 15);
select * from Kniha where Id in (14, 15, 16, 17);
select * from Souvisejici where IdKniha in (14, 15, 16, 17);



-- Testy procedur a funkcí
--------------------------

-- Procedura UpravPocetVKosiku (@IdKniha int, @IdObjednavka bigint output, @Zmena int)

-- Prázdný košík (objednávka s id 8)
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Pøidáme do košíku 13 kusù knihy Rezistence s Id 6
execute UpravPocetVKosiku 6, 8, 13;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Odebereme 5 kusù knihy
execute UpravPocetVKosiku 6, 8, -5;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - pokusíme se odebrat z objednávky pøíliš velké množství (10 kusù, v objednávce je jich ale jen 8)
execute UpravPocetVKosiku 6, 8, -10;
-- Odebereme zbylých 8 kusù
execute UpravPocetVKosiku 6, 8, -8;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - pøidáváme do neexistující objednávky s id 666
execute UpravPocetVKosiku 6, 666, 1;
-- Chyba - pøidáváme do objednávky s id 1, která už není ve stavu 'Kosik'
execute UpravPocetVKosiku 6, 1, 1;
-- Vytvoøení košíku, pokud není ještì vytvoøený
declare @x bigint = NULL;
execute UpravPocetVKosiku 6, @x output, 1;
select * from Objednavka_VW where Id = @x;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = @x;


-- Procedura PotvrdObjednavku (@IdObjednavka bigint, @IdTypDopravy int, @IdTypUhrady int, @Jmeno nvarchar(30), @Prijmeni nvarchar(30), @Ulice nvarchar(30), @Mesto nvarchar(30), @Psc char(5), @Email nvarchar(60))

-- Košík s jednou položkou (objednávka s id 5), na skladì 4, v objednávce 3
select * from Objednavka_VW where Id = 5;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 5;
-- Potvrzení objednávky (na skladì zùstane 1 kus)
execute PotvrdObjednavku 5, 1, 1, 'Jmeno', 'Prijmeni', 'Ulice', 'Mesto', '10000', 'email@gmail.com'
select * from Objednavka_VW where Id = 5;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 5;
-- Prázdný košík (objednávka s id 8)
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - prázdný košík nelze potvrdit
execute PotvrdObjednavku 8, 1, 1, 'Jmeno', 'Prijmeni', 'Ulice', 'Mesto', '10000', 'email@gmail.com'
-- Vložíme do košíku tolik knih, že nebudou na skladì
execute UpravPocetVKosiku 6, 8, 1;
execute UpravPocetVKosiku 4, 8, 10000;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - objednávka nelze potvrdit, protože není dostateèný poèet položek na skladì, objednávka zùstane nepotvrzená (ve stavu 'Kosik')
execute PotvrdObjednavku 8, 1, 1, 'Jmeno', 'Prijmeni', 'Ulice', 'Mesto', '10000', 'email@gmail.com'


-- Procedura StornujObjednavku(@IdObjednavka bigint)

-- Potvrzená objednávka
select * from Objednavka_VW where Id = 3;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 3;
-- Objednávku stornujeme, knihy se vrátí na sklad
execute StornujObjednavku 3;
select * from Objednavka_VW where Id = 3;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 3;
-- Chyba - Objednávku nejde znovu stornovat
execute StornujObjednavku 3;


-- Procedura OznacObjednavkuZaplacena(@IdObjednavka bigint)

-- Potvrzená nezaplacená objednávka
select * from Objednavka_VW where Id = 1;
-- Oznaèení jako zaplacená
execute OznacObjednavkuZaplacena 1;
select * from Objednavka_VW where Id = 1;
-- Chyba - pøi druhém pokusu ji oznaèit neprojde
execute OznacObjednavkuZaplacena 1;
-- Odeslaná objednávka s id 2
select * from Objednavka_VW where Id = 2;
-- Chyba - nelze zaplatit objednávku, která je v jiném stavu než 'Potvrzena'
execute OznacObjednavkuZaplacena 2;


-- Procedura OznacObjednavkuNezaplacena(@IdObjednavka bigint)

-- stornovaná zaplacená objednávka
select * from Objednavka_VW where Id = 4;
-- Peníze byly vráceny zákazníkovi zpátky -> nebude zaplacená
execute OznacObjednavkuNezaplacena 4;
select * from Objednavka_VW where Id = 4;
-- Chyba - peníze nejde vrátit dvakrát, takže nejde ani oznaèit jako nezaplacenou objednávku, která je oznaèena jako nezaplacená
execute OznacObjednavkuNezaplacena 4;


-- Procedura OznacObjednavkuOdeslana(@IdObjednavka bigint)

-- potvrzená nezaplacená objednávka
select * from Objednavka_VW where Id = 5;
-- Chyba - nezaplacená objednávka nejde odeslat
execute OznacObjednavkuOdeslana 5;
-- Objednávku oznaèíme za zaplacenou a pak ji už mùžeme odeslat
execute OznacObjednavkuZaplacena 5;
execute OznacObjednavkuOdeslana 5;
select * from Objednavka_VW where Id = 5;


-- Procedura ObjednanoUDodavatele(@IdKniha bigint, @Pocet int)

-- Máme knihu a u ní málo kusù na skladì
select * from Detail_Knihy_VW where Id = 11;
-- Objednáme dalších 10 kusù knihy
execute ObjednanoUDodavatele 11, 10;
select * from Detail_Knihy_VW where Id = 11;
-- zjistili jsme, že jsme objednali moc a èást objednávky (5 kusù) u dodavatele zrušíme
execute ObjednanoUDodavatele 11, -5;
select * from Detail_Knihy_VW where Id = 11;
-- Chyba - nemùžeme zrušit víc než je objednáno
execute ObjednanoUDodavatele 11, -50;


-- Procedura PrisloOdDodavatele(@IdKniha bigint, @Pocet int)

-- Máme knihu a u ní objednané kusy
select * from Detail_Knihy_VW where Id = 5;
-- Kusy pøišly od dodavatele, tak je pøesuneme na sklad
execute PrisloOdDodavatele 5, 112
select * from Detail_Knihy_VW where Id = 5;
-- Chyba - nejde pøesunout, co nebylo objednáno
execute PrisloOdDodavatele 5, 112


-- Funkce CenaObjednavky(@IdObjednavka bigint)

-- Polozky objednavky s id 2
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 2;
-- Jeji celkova cena
select dbo.CenaObjednavky(2);
-- Objednávka bez položek má nulovou cenu
select * from Objednavka_VW where Id = 1002;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 1002;
select dbo.CenaObjednavky(1002);



-- Testy pohledù
----------------

-- Seznam kategorií a podkategorií pro výpis v menu
select * from Seznam_Kategorii_VW order by Hlavni_Nazev, Podkategorie_Nazev;

-- Stránka s výpisem všech autorù seøazených podle jména a poètù jejich knih
select * from Seznam_Autoru_VW order by Prijmeni, Jmeno;

-- Stránka s výpisem všech nakladatelství seøazených podle názvu a poètu pøíslušných knih
select * from Seznam_Nakladatelstvi_VW order by Nazev;

-- Stránka s výpisem všech jazykù seøazených podle názvu a poètu pøíslušných knih
select * from Seznam_Jazyku_VW order by Nazev;

-- Stránka s výpisem knih v kategorii s id = 1 (øazení podle názvu)
select * from Vypis_Knih_Kategorie_VW where IdKategorie = 1 order by Nazev;

-- Stránka s výpisem knih v kategorii s id = 1 (øazení podle ceny)
select * from Vypis_Knih_Kategorie_VW where IdKategorie = 1 order by Cena;

-- Stránka s výpisem knih autora s id = 10 (øazení podle názvu)
select * from Vypis_Knih_Autor_VW where IdAutor = 10 order by Nazev;

-- Stránka s výpisem knih autora s id = 10 (øazení podle ceny)
select * from Vypis_Knih_Autor_VW where IdAutor = 10 order by Cena;

-- Stránka s výpisem knih z nakladatelství s id = 6 (øazení podle názvu)
select * from Vypis_Knih_Nakladatelstvi_VW where IdNakladatelstvi = 6 order by Nazev;

-- Stránka s výpisem knih z nakladatelství s id = 6 (øazení podle ceny)
select * from Vypis_Knih_Nakladatelstvi_VW where IdNakladatelstvi = 6 order by Cena;

-- Stránka s výpisem knih v jazyku s id = 2 (øazení podle názvu)
select * from Vypis_Knih_Jazyk_VW where IdJazyk = 2 order by Nazev;

-- Stránka s výpisem knih v jazyku s id = 2 (øazení podle ceny)
select * from Vypis_Knih_Jazyk_VW where IdJazyk = 2 order by Cena;

-- Stránka s detailem knihy s id = 2
select * from Detail_Knihy_VW where Id = 2;

-- Výpis souvisejících knih pro knihu s id = 2, seøazené podle názvu
select * from Souvisejici_Knihy_VW where IdKniha = 2 order by Nazev;

-- Výpis základních informací z objednávky s id = 3
select * from Objednavka_VW where Id = 3;

-- Výpis položek objednávky pro objednávku s id = 3
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 3;

-- Seznam potvrzených nezaplacených objednávek
select * from Potvrzene_Nezaplacene_Objednavky_VW;

-- Seznam potvrzených zaplacených objednávek
select * from Potvrzene_Zaplacene_Objednavky_VW;

-- Seznam stornovaných zaplacených objednávek
select * from Stornovane_Zaplacene_Objednavky_VW;

-- Seznam docházejících knih (výhledovì dostupné = na skladu + objednáno < 5), seøazeno podle výhledovì dostupných
select * from Dochazejici_Knihy_VW order by VyhledoveDostupne;

-- Seznam knih s prodejností, seøazený podle prodejnosti
select * from Prodejnost_Kniha_VW order by ProdanoKusu;

-- Seznam autorù s prodejností, seøazený podle prodejnosti
select * from Prodejnost_Autor_VW order by ProdanoKusu;

-- Seznam souvisejících knih druhého øádu pro danou knihu s id = 1
select * from Souvisejici_Souvisejicich_VW where IdPuvodniKniha = 1;
