-- Internetov� obchod s knihami

-- Testovac� p��klady

-- Pro funk�nost je pot�eba nejprve datab�zi naplnit demonstra�n�mi daty v odd�len�m souboru.
-- Jednotliv� p��kazy v tomto skriptu p�edpokl�daj� spu�t�n� v�ech p�edch�zej�c�ch nechybov�ch p��kaz�.

-- Testy integritn�ch omezen�
-----------------------------

-- Vzor spr�vn�ch knih, kter� jdou vlo�it a na kter�ch budou uk�z�ny integritn� omezen�
insert into Kniha values (12, 1, 1, 1, 1, 'N�zev knihy', 'Popis knihy', NULL, 153, '15-54345-212-4', 5, 5, 199.00);
insert into Kniha values (13, 2, 2, 2, 2, 'N�zev jin� knihy', 'Popis jin� knihy', 2005, NULL, '15-54345-212-5', 2, 3, 299.00);
-- Chyba - u knihy nen� mo�n� nastavit nesmysln� rok vyd�n�
update Kniha set RokVydani = 45454 where Id = 12
-- Chyba - u knihy nen� mo�n� nastavit duplicitn� ISBN
update Kniha set ISBN = '15-54345-212-5' where Id = 12
-- Chyba - u knihy nen� mo�n� nastavit neplatn� ISBN
update Kniha set ISBN = 'ISBN: 15-54345-212-5' where Id = 12
-- Chyba - cena knihy mus� b�t nez�porn�
update Kniha set Cena = -258 where Id = 12
-- Chyba - skladov� z�soby mus� b�t nez�porn�
update Kniha set PocetNaSkladu = PocetNaSkladu - 50 where Id = 12
-- Chyba - autor s dan�m Id neexistuje
update Kniha set IdAutor = 5453 where Id = 12

-- Chyba - nejde vlo�it k jedn� knize jinou jako souvisej�c� dvakr�t
insert into Souvisejici values (12, 13), (12, 13);

-- Je mo�n� do r�zn�ch hlavn�ch kategori� vlo�it stejn� nazvan� podkategorie
insert into Kategorie values (500, NULL, 'Hlavn� 1', NULL),
              (501, NULL, 'Hlavn� 2', NULL),
              (502, 500, 'Podkategorie', NULL),
              (503, 501, 'Podkategorie', NULL);
-- Chyba - nen� mo�n� vlo�it do jedn� hlavn� kategorie dv� stejn� pojmenovan� podkategorie
insert into Kategorie values (504, NULL, 'Hlavn�', NULL),
              (505, 504, 'Podkategorie', NULL),
              (506, 504, 'Podkategorie', NULL);

-- Vytvo�en� nov� objedn�vky ve stavu ko��ku
insert into Objednavka values (7, 'Kosik', NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL);
-- Chyba - objedn�vku ve stavu 'Kosik' nejde zaplatit
update Objednavka set Zaplacena = 1 where Id = 7;
-- Chyba - objedn�vka v jin�m stavu ne� 'Kosik' mus� m�t vypln�n� v�echny �daje
update Objednavka set Stav = 'Potvrzena' where Id = 7;

-- P��klad odeslan� objedn�vky
select * from Objednavka where Id = 2;
-- Chyba - odeslan� objedn�vka nem��e b�t nezaplacen�
update Objednavka set Zaplacena = 0 where Id = 2;
-- Chyba - objedn�vka mus� m�t PS� slo�en� pouze z ��slic a d�lku 5
update Objednavka set Psc = '143 00' where Id = 2;
update Objednavka set Psc = '1430' where Id = 2;

-- P��klad - do pr�zdn�ho ko��ku je vlo�ena polo�ka
insert into PolozkaObjednavky values (7, 1, 3);
select * from PolozkaObjednavky where IdObjednavka = 7;
-- Objedn�vka jde smazat, p�esto�e v sob� m� polo�ku
delete from Objednavka where Id = 7;
-- Polo�ka z objedn�vky byla kask�dov� smaz�na
select * from PolozkaObjednavky where IdObjednavka = 7;



-- Testy trigger�
-----------------

-- Hlavn� kategorie s Id = 1 m� podkategorii s Id 7
select * from Kategorie where Id in (1, 7);
-- Vytvo��me novou kategorii bez nad�azen� kategorie
insert into Kategorie values (20, NULL, 'Kat', NULL);
-- Chyba - nejde novou kategorii za�adit nad dv� kategorie (kategorie jsou max dvou�rov�ov�)
update Kategorie set IdNadrazenaKategorie = 20 where Id = 1;
-- Chyba - nejde novou kategorii za�adit pod dv� kategorie
update Kategorie set IdNadrazenaKategorie = 7 where Id = 20;

-- Vytvo�en� kategorie s dv�ma podkategoriemi
insert into Kategorie values (21, NULL, '21', NULL),
              (22, 21, '22', NULL),
              (23, 21, '23', NULL);
select * from Kategorie where Id in (21, 22, 23);
-- Odstran�n� jedn� podkategorie
delete from Kategorie where Id = 22;
select * from Kategorie where Id in (21, 22, 23);
-- Odstran�n� nad�azen� kategorie (je s n� odstran�na automaticky i podkategorie pomoc� triggeru)
delete from Kategorie where Id = 21;
select * from Kategorie where Id in (21, 22, 23);

-- Vytvo�en� �ty� vz�jemn� souvisej�c�ch knih
insert into Kniha values (14, 1, 1, 1, 1, '14', 'Popis 14', NULL, 153, '15-54345-212-14', 5, 5, 199.00);
insert into Kniha values (15, 2, 2, 2, 2, '15', 'Popis 15', 2005, NULL, '15-54345-212-15', 2, 3, 299.00);
insert into Kniha values (16, 2, 2, 2, 2, '16', 'Popis 16', 2005, NULL, '15-54345-212-16', 2, 3, 299.00);
insert into Kniha values (17, 2, 2, 2, 2, '17', 'Popis 17', 2005, NULL, '15-54345-212-17', 2, 3, 299.00);
insert into Souvisejici values (14, 15), (15, 14), (16, 14), (14, 16), (16, 15), (15, 16), (14, 17), (15, 17), (16, 17), (17, 14), (17, 15), (17, 16);
select * from Kniha where Id in (14, 15, 16, 17);
select * from Souvisejici where IdKniha in (14, 15, 16, 17);
-- Odstran�n� dvou knih (jsou automaticky odstran�ny i polo�ky v souvisej�c�ch)
delete from Kniha where Id in (14, 15);
select * from Kniha where Id in (14, 15, 16, 17);
select * from Souvisejici where IdKniha in (14, 15, 16, 17);



-- Testy procedur a funkc�
--------------------------

-- Procedura UpravPocetVKosiku (@IdKniha int, @IdObjednavka bigint output, @Zmena int)

-- Pr�zdn� ko��k (objedn�vka s id 8)
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- P�id�me do ko��ku 13 kus� knihy Rezistence s Id 6
execute UpravPocetVKosiku 6, 8, 13;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Odebereme 5 kus� knihy
execute UpravPocetVKosiku 6, 8, -5;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - pokus�me se odebrat z objedn�vky p��li� velk� mno�stv� (10 kus�, v objedn�vce je jich ale jen 8)
execute UpravPocetVKosiku 6, 8, -10;
-- Odebereme zbyl�ch 8 kus�
execute UpravPocetVKosiku 6, 8, -8;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - p�id�v�me do neexistuj�c� objedn�vky s id 666
execute UpravPocetVKosiku 6, 666, 1;
-- Chyba - p�id�v�me do objedn�vky s id 1, kter� u� nen� ve stavu 'Kosik'
execute UpravPocetVKosiku 6, 1, 1;
-- Vytvo�en� ko��ku, pokud nen� je�t� vytvo�en�
declare @x bigint = NULL;
execute UpravPocetVKosiku 6, @x output, 1;
select * from Objednavka_VW where Id = @x;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = @x;


-- Procedura PotvrdObjednavku (@IdObjednavka bigint, @IdTypDopravy int, @IdTypUhrady int, @Jmeno nvarchar(30), @Prijmeni nvarchar(30), @Ulice nvarchar(30), @Mesto nvarchar(30), @Psc char(5), @Email nvarchar(60))

-- Ko��k s jednou polo�kou (objedn�vka s id 5), na sklad� 4, v objedn�vce 3
select * from Objednavka_VW where Id = 5;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 5;
-- Potvrzen� objedn�vky (na sklad� z�stane 1 kus)
execute PotvrdObjednavku 5, 1, 1, 'Jmeno', 'Prijmeni', 'Ulice', 'Mesto', '10000', 'email@gmail.com'
select * from Objednavka_VW where Id = 5;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 5;
-- Pr�zdn� ko��k (objedn�vka s id 8)
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - pr�zdn� ko��k nelze potvrdit
execute PotvrdObjednavku 8, 1, 1, 'Jmeno', 'Prijmeni', 'Ulice', 'Mesto', '10000', 'email@gmail.com'
-- Vlo��me do ko��ku tolik knih, �e nebudou na sklad�
execute UpravPocetVKosiku 6, 8, 1;
execute UpravPocetVKosiku 4, 8, 10000;
select * from Objednavka_VW where Id = 8;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 8;
-- Chyba - objedn�vka nelze potvrdit, proto�e nen� dostate�n� po�et polo�ek na sklad�, objedn�vka z�stane nepotvrzen� (ve stavu 'Kosik')
execute PotvrdObjednavku 8, 1, 1, 'Jmeno', 'Prijmeni', 'Ulice', 'Mesto', '10000', 'email@gmail.com'


-- Procedura StornujObjednavku(@IdObjednavka bigint)

-- Potvrzen� objedn�vka
select * from Objednavka_VW where Id = 3;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 3;
-- Objedn�vku stornujeme, knihy se vr�t� na sklad
execute StornujObjednavku 3;
select * from Objednavka_VW where Id = 3;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 3;
-- Chyba - Objedn�vku nejde znovu stornovat
execute StornujObjednavku 3;


-- Procedura OznacObjednavkuZaplacena(@IdObjednavka bigint)

-- Potvrzen� nezaplacen� objedn�vka
select * from Objednavka_VW where Id = 1;
-- Ozna�en� jako zaplacen�
execute OznacObjednavkuZaplacena 1;
select * from Objednavka_VW where Id = 1;
-- Chyba - p�i druh�m pokusu ji ozna�it neprojde
execute OznacObjednavkuZaplacena 1;
-- Odeslan� objedn�vka s id 2
select * from Objednavka_VW where Id = 2;
-- Chyba - nelze zaplatit objedn�vku, kter� je v jin�m stavu ne� 'Potvrzena'
execute OznacObjednavkuZaplacena 2;


-- Procedura OznacObjednavkuNezaplacena(@IdObjednavka bigint)

-- stornovan� zaplacen� objedn�vka
select * from Objednavka_VW where Id = 4;
-- Pen�ze byly vr�ceny z�kazn�kovi zp�tky -> nebude zaplacen�
execute OznacObjednavkuNezaplacena 4;
select * from Objednavka_VW where Id = 4;
-- Chyba - pen�ze nejde vr�tit dvakr�t, tak�e nejde ani ozna�it jako nezaplacenou objedn�vku, kter� je ozna�ena jako nezaplacen�
execute OznacObjednavkuNezaplacena 4;


-- Procedura OznacObjednavkuOdeslana(@IdObjednavka bigint)

-- potvrzen� nezaplacen� objedn�vka
select * from Objednavka_VW where Id = 5;
-- Chyba - nezaplacen� objedn�vka nejde odeslat
execute OznacObjednavkuOdeslana 5;
-- Objedn�vku ozna��me za zaplacenou a pak ji u� m��eme odeslat
execute OznacObjednavkuZaplacena 5;
execute OznacObjednavkuOdeslana 5;
select * from Objednavka_VW where Id = 5;


-- Procedura ObjednanoUDodavatele(@IdKniha bigint, @Pocet int)

-- M�me knihu a u n� m�lo kus� na sklad�
select * from Detail_Knihy_VW where Id = 11;
-- Objedn�me dal��ch 10 kus� knihy
execute ObjednanoUDodavatele 11, 10;
select * from Detail_Knihy_VW where Id = 11;
-- zjistili jsme, �e jsme objednali moc a ��st objedn�vky (5 kus�) u dodavatele zru��me
execute ObjednanoUDodavatele 11, -5;
select * from Detail_Knihy_VW where Id = 11;
-- Chyba - nem��eme zru�it v�c ne� je objedn�no
execute ObjednanoUDodavatele 11, -50;


-- Procedura PrisloOdDodavatele(@IdKniha bigint, @Pocet int)

-- M�me knihu a u n� objednan� kusy
select * from Detail_Knihy_VW where Id = 5;
-- Kusy p�i�ly od dodavatele, tak je p�esuneme na sklad
execute PrisloOdDodavatele 5, 112
select * from Detail_Knihy_VW where Id = 5;
-- Chyba - nejde p�esunout, co nebylo objedn�no
execute PrisloOdDodavatele 5, 112


-- Funkce CenaObjednavky(@IdObjednavka bigint)

-- Polozky objednavky s id 2
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 2;
-- Jeji celkova cena
select dbo.CenaObjednavky(2);
-- Objedn�vka bez polo�ek m� nulovou cenu
select * from Objednavka_VW where Id = 1002;
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 1002;
select dbo.CenaObjednavky(1002);



-- Testy pohled�
----------------

-- Seznam kategori� a podkategori� pro v�pis v menu
select * from Seznam_Kategorii_VW order by Hlavni_Nazev, Podkategorie_Nazev;

-- Str�nka s v�pisem v�ech autor� se�azen�ch podle jm�na a po�t� jejich knih
select * from Seznam_Autoru_VW order by Prijmeni, Jmeno;

-- Str�nka s v�pisem v�ech nakladatelstv� se�azen�ch podle n�zvu a po�tu p��slu�n�ch knih
select * from Seznam_Nakladatelstvi_VW order by Nazev;

-- Str�nka s v�pisem v�ech jazyk� se�azen�ch podle n�zvu a po�tu p��slu�n�ch knih
select * from Seznam_Jazyku_VW order by Nazev;

-- Str�nka s v�pisem knih v kategorii s id = 1 (�azen� podle n�zvu)
select * from Vypis_Knih_Kategorie_VW where IdKategorie = 1 order by Nazev;

-- Str�nka s v�pisem knih v kategorii s id = 1 (�azen� podle ceny)
select * from Vypis_Knih_Kategorie_VW where IdKategorie = 1 order by Cena;

-- Str�nka s v�pisem knih autora s id = 10 (�azen� podle n�zvu)
select * from Vypis_Knih_Autor_VW where IdAutor = 10 order by Nazev;

-- Str�nka s v�pisem knih autora s id = 10 (�azen� podle ceny)
select * from Vypis_Knih_Autor_VW where IdAutor = 10 order by Cena;

-- Str�nka s v�pisem knih z nakladatelstv� s id = 6 (�azen� podle n�zvu)
select * from Vypis_Knih_Nakladatelstvi_VW where IdNakladatelstvi = 6 order by Nazev;

-- Str�nka s v�pisem knih z nakladatelstv� s id = 6 (�azen� podle ceny)
select * from Vypis_Knih_Nakladatelstvi_VW where IdNakladatelstvi = 6 order by Cena;

-- Str�nka s v�pisem knih v jazyku s id = 2 (�azen� podle n�zvu)
select * from Vypis_Knih_Jazyk_VW where IdJazyk = 2 order by Nazev;

-- Str�nka s v�pisem knih v jazyku s id = 2 (�azen� podle ceny)
select * from Vypis_Knih_Jazyk_VW where IdJazyk = 2 order by Cena;

-- Str�nka s detailem knihy s id = 2
select * from Detail_Knihy_VW where Id = 2;

-- V�pis souvisej�c�ch knih pro knihu s id = 2, se�azen� podle n�zvu
select * from Souvisejici_Knihy_VW where IdKniha = 2 order by Nazev;

-- V�pis z�kladn�ch informac� z objedn�vky s id = 3
select * from Objednavka_VW where Id = 3;

-- V�pis polo�ek objedn�vky pro objedn�vku s id = 3
select * from Vypis_Polozek_Objednavky_VW where IdObjednavka = 3;

-- Seznam potvrzen�ch nezaplacen�ch objedn�vek
select * from Potvrzene_Nezaplacene_Objednavky_VW;

-- Seznam potvrzen�ch zaplacen�ch objedn�vek
select * from Potvrzene_Zaplacene_Objednavky_VW;

-- Seznam stornovan�ch zaplacen�ch objedn�vek
select * from Stornovane_Zaplacene_Objednavky_VW;

-- Seznam doch�zej�c�ch knih (v�hledov� dostupn� = na skladu + objedn�no < 5), se�azeno podle v�hledov� dostupn�ch
select * from Dochazejici_Knihy_VW order by VyhledoveDostupne;

-- Seznam knih s prodejnost�, se�azen� podle prodejnosti
select * from Prodejnost_Kniha_VW order by ProdanoKusu;

-- Seznam autor� s prodejnost�, se�azen� podle prodejnosti
select * from Prodejnost_Autor_VW order by ProdanoKusu;

-- Seznam souvisej�c�ch knih druh�ho ��du pro danou knihu s id = 1
select * from Souvisejici_Souvisejicich_VW where IdPuvodniKniha = 1;
