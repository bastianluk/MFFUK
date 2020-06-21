-- Internetov� obchod s knihami

-- Zad�n� �lohy a vytvo�en� tabulek


/*
Internetov� obchod s knihami
----------------------------

Internetov� obchod prod�v� pouze knihy. Ka�d� kniha m� autora (ten m� jm�no a p��jmen�, jm�no nemus� b�t vypln�no), vydalo ji nakladatelstv� (to m� n�zev a adresu), je napsan� v n�jak�m jazyku, je za�azena do n�jak� jedn� kategorie knih, m� n�zev, ISBN, evidujeme u n� po�et knih na skladu, po�et objednan�ch knih od dodavatele a cenu. Voliteln� u knihy je popis, rok vyd�n� a po�et stran. Pro ka�dou knihu evidujeme, kter� knihy jsou k n� souvisej�c� (relace nemus� b�t symetrick�, obvykle pro jednu knihu budou maxim�ln� jednotky souvisej�c�ch knih, des�tky velmi ojedin�le).

Knihy jsou v obchod� uspo��d�ny v dvou�rov�ov�ch kategori�ch - jsou hlavn� kategorie a ka�d� m��e m�t podkategorie. Knihy mohou n�le�et jak do hlavn� kategorie (to je vyu�ito, pokud kniha z�ejm� pat�� do dan� hlavn� kategorie, ale neexistuje ��dn� vhodn� podkategorie), tak do podkategorie. Ka�d� kategorie m� n�zev a voliteln� popis. N�zvy kategori� jsou unik�tn� pouze v dan� kategorii (tj. nemohou se jmenovat stejn� dv� hlavn� kategorie a nemohou se jmenovat stejn� dv� podkategorie v jedn� hlavn� kategorii).


Spr�va objedn�vky
-----------------

Ka�d� objedn�vka m� p�id�len� n�hodn� tajn� unik�tn� k�d. Ten se nap�. ukl�d� v cookies, aby u�ivatel m�l uchovan� stav ko��ku mezi n�v�t�vami. Po potvrzen� objedn�vky slou�� k�d ke sledov�n� stavu objedn�vky (z�kazn�kovi je na e-mail odesl�n odkaz pro sledov�n� objedn�vky, kde se m��e pod�vat na obsah objedn�vky a zda je objedn�vka evidov�na jako zaplacen� a jako odeslan� a kde m��e objedn�vku je�t� stornovat, pokud nebyla odeslan�).

V�choz� stav je, �e u�ivatel nem� vytvo�enu ��dnou objedn�vku se stavem "Kosik". Ve chv�li, kdy p�id� polo�ku do ko��ku (v libovoln�m mno�stv�, stav skladu se bude kontrolovat a� p�i potvrzov�n� objedn�vky), se mu vytvo�� objedn�vka ve stavu "Kosik" (p�i odebr�n� posledn� polo�ky z ko��ku se takov� objedn�vka u� neodstra�uje). Ve chv�li, kdy u�ivatel bude cht�t vybran� polo�ky v ko��ku koupit, zad� v�echny pot�ebn� �daje (typ dopravy, typ platby, jm�no, p��jmen�, ulici, m�sto, PS�, e-mailovou adresu) a pokud jsou v�echny vybran� knihy dostupn�, tak se stav objedn�vky zm�n� na "Potvrzena" a knihy se odeberou ze skladov�ch z�sob. Polo�ky objedn�vky lze editovat pouze ve stavu "Kosik".

Pokud z�kazn�k chce objedn�vku stornovat p�ed odesl�n�m, m��e p�ev�st stav objedn�vky z "Potvrzena" na "Stornovana", knihy se zp�tky p�i�tou do skladov�ch z�sob. Po odesl�n� knih z�kazn�kovi (obchod p�ij�m� pouze platby p�edem, tak�e objedn�vka se ode�le teprve po zaplacen�) se stav objedn�vky zm�n� na "Odeslana".


Zobrazovan� str�nky pro n�v�t�vn�ky
-----------------------------------

- menu - v�pis v�ech kategori� a podkagegori�, zobrazuje se v�dy, ka�d� polo�ka m� pouze n�zev

- v�pis autor� - zobrazeni v�ichni auto�i s informac� o po�tu r�zn�ch knih od ka�d�ho autora, �azen� podle p��jmen� a jm�na autora
- v�pis nakladatelstv� - zobrazeny v�echny n�zvy (a v kontextov� n�pov�d� adresy) nakladatelstv� s informac� o po�tu r�zn�ch knih od ka�d�ho nakladatelstv�, �azen� podle n�zvu nakladatelstv�
- v�pis jazyk� - zobrazeny v�echny jazyky s informac� o po�tu r�zn�ch knih od ka�d�ho jazyka, �azen� podle n�zvu jazyka
- v�pis knih (pro kategorii, autora, nakladatelstv� nebo jazyk) - u knih zobrazen n�zev, autor (krom� zobrazen� podle autora) a cena, �adit je mo�n� podle n�zvu knihy nebo ceny
- zobrazen� jedn� knihy - zobrazeny v�echny relevantn� dostupn� informace o konkr�tn� knize, zobrazeny v�echny souvisej�c� knihy (pouze n�zev, autor a cena, �azen� podle n�zvu)
- zobrazen� ko��ku - v�pis v�ech polo�ek v ko��ku, u ka�d� polo�ky se zobrazuje n�zev knihy, autor, po�et kus�, cena a informace, zda je dostate�n� po�et kus� na sklad�
- sledov�n� objedn�vky - v�pis v�ech polo�ek v ko��ku, u ka�d� polo�ky se zobrazuje n�zev knihy, autor, po�et kus� a cena

Administrace webu
-----------------

B�n� zobrazen� pro n�v�t�vn�ky bude dopln�no o administra�n� odkazy (odebr�n� polo�ky, editace polo�ky, p�id�n� polo�ky). Nav�c budou n�kter� str�nky administrace.

Zam�stnanci obchodu budou moct p�id�vat, upravovat, odstra�ovat
  - autory
  - nakladatelstv�
  - jazyky
  - kategorie
  - knihy
  - souvisej�c� knihy
Tyto editace se budou prov�d�t sp�e ojedin�le (vyd�n� nov� knihy, oprava chybn� zadan�ho �daje, �prava souvisej�c�ch knih, ...), bude proto mo�n� prov�d�t �pravy libovoln�ch dostupn�ch �daj� entit.

P�i b�n�m provozu obchodu budou zam�stnanci vyu��vat hlavn� tyto zobrazen� a mo�nosti:

(vy�izov�n� objedn�vek)
- Zobrazit seznam potvrzen�ch nezaplacen�ch objedn�vek
  - Mo�nost ozna�it objedn�vku jako zaplacenou
  - Mo�nost zm�nit stav na "Stornovana" (nap�. u�ivatel psal e-mail a ztratil u� k�d objedn�vky)
- Zobrazit seznam potvrzen�ch zaplacen�ch objedn�vek
  - Mo�not zm�nit stav na "Odeslana" (po zabalen� a odesl�n� knih)
  - Mo�nost zm�nit stav na "Stornovana" (nap�. u�ivatel psal e-mail a ztratil u� k�d objedn�vky, za�lou se pen�ze zp�t)
- Zobrazit seznam stornovan�ch zaplacen�ch objedn�vek
  - Mo�not zm�nit objedn�vku na nezaplacenou (po vr�cen� pen�z z�kazn�kovi)
- Zobrazit detail objedn�vky - polo�ky objedn�vky s ISBN (zobrazen� podobn� zobrazen� ko��ku pro z�kazn�ka, slou�� jako seznam knih, kter� je pot�eba naj�t ve skladu, zabalit a odeslat, ve skladu jsou knihy se�azen� podle ISBN)

(objedn�v�n� knih u dodavatele a nasklad�ov�n�)
- Zobrazit knihy s mal�m po�tem v�hledov� dostupn�ch knih (na skladu + objedn�no < 5), se�azen� vzestupn� podle po�tu v�hledov� dostupn�ch knih, d�le�it� je ISBN, podle kter�ho se bude objedn�vat u dodavatele
- Vyhledat knihu podle ISBN
- Mo�nost u knihy zv�t�it po�et objednan�ch knih (kdy� zam�stnanec objednal knihu u dodavatele)
- Mo�nost u knihy p�esunout ur�it� po�et z objednan�ch do skladov�ch (kdy� knihy p�ijdou od dodavatele)

Obchod bude tak� umo��ovat sledovat ur�it� statistiky a zaj�mav� �daje:
- V�pis knih s po�tem prodan�ch kus� (pro nalezen� nejprod�van�j��ch a neprod�van�ch knih)
- V�pis autor� s po�tem prodan�ch kus�
- Zobrazen� souvisej�c�ch knih souvisej�c�ch knih, kter� nejsou p��mo souvisej�c� (to se hod� v p��pad�, �e m�me knihu a k n� m�lo souvisej�c�ch knih, je pravd�podobn�, �e souvisej�c� knihy souvisej�c�ch knih budou souviset i s p�vodn� knihou)
- Seznam potvrzen�ch, ale nezaplacen�ch objedn�vek, kter� jsou star�� ne� m�s�c (potvrzen� objedn�vka blokuje knihy, star� nezaplacen� objedn�vky se m��ou stornovat)
*/


CREATE TABLE Autor (
  Id int
    constraint Autor_PK PRIMARY KEY,
  Jmeno nvarchar(50),
  Prijmeni nvarchar(50) NOT NULL,
  constraint Autor_U_Prijmeni_Jmeno UNIQUE (Prijmeni, Jmeno)
  );

CREATE TABLE Nakladatelstvi (
  Id int
    constraint Nakladatelstvi_PK PRIMARY KEY,
  Nazev nvarchar(50) NOT NULL
    constraint Nakladatelstvi_U_Nazev UNIQUE,
  Adresa nvarchar(200)
  );

CREATE TABLE Jazyk (
  Id int
    constraint Jazyk_PK PRIMARY KEY,
  Nazev nvarchar(50) NOT NULL
    constraint Jazyk_U_Nazev UNIQUE,
  );

CREATE TABLE Kategorie (
  Id int
    constraint Kategorie_PK PRIMARY KEY,
  IdNadrazenaKategorie int
    constraint Kategorie_FK_NadrazenaKategorie REFERENCES Kategorie(Id),
    -- on delete cascade - implementovano triggerem kvuli cyklicke referenci
  Nazev nvarchar(50) NOT NULL,
  Popis nvarchar(max),
  constraint Kategorie_U_NazevVKategorii UNIQUE (Nazev, IdNadrazenaKategorie)
  );

CREATE TABLE Kniha(
  Id int
    constraint Kniha_PK PRIMARY KEY,
  IdKategorie int NOT NULL
    constraint Kniha_FK_Kategorie REFERENCES Kategorie(Id),
  IdAutor int NOT NULL
    constraint Kniha_FK_Autor REFERENCES Autor(Id),
  IdNakladatelstvi int NOT NULL
    constraint Kniha_FK_Nakladatelstvi REFERENCES Nakladatelstvi(Id),
  IdJazyk int NOT NULL
    constraint Kniha_FK_Jazyk REFERENCES Jazyk(Id),
  Nazev nvarchar(150) NOT NULL,
  Popis nvarchar(max),
  RokVydani int
    constraint Kniha_CHK_RokVydani
      check (RokVydani >= 0 AND RokVydani <= 2100),
  PocetStran int
    constraint Kniha_CHK_PocetStran
      check (PocetStran > 0),
  ISBN nchar(30) NOT NULL
    constraint Kniha_CHK_ISBN
      check (ISBN like replicate('[- 0-9X]', 30))
    constraint Kniha_U_ISBN UNIQUE,
  PocetNaSkladu int NOT NULL
    constraint Kniha_CHK_PocetNaSkladu
      check (PocetNaSkladu >= 0),
  PocetObjednano int NOT NULL
    constraint Kniha_CHK_PocetObjednano
      check (PocetObjednano >= 0),
  Cena numeric(10, 2) NOT NULL
    constraint Kniha_CHK_Cena
      check (Cena >= 0)
  );

CREATE TABLE Souvisejici(
  IdKniha int NOT NULL
    constraint Souvisejici_FK_Kniha REFERENCES Kniha(Id),
    -- on delete cascade - implementovano triggerem kvuli cyklicke referenci
  IdSouvisejiciKniha int NOT NULL
    constraint Souvisejici_FK_SouvisejiciKniha REFERENCES Kniha(Id),
    -- on delete cascade - implementovano triggerem kvuli cyklicke referenci
  constraint Souvisejici_PK PRIMARY KEY (IdKniha, IdSouvisejiciKniha)
  );

CREATE TABLE TypDopravy(
  Id int
    constraint TypDopravy_PK PRIMARY KEY,
  Doprava nvarchar(30) NOT NULL
    constraint TypDopravy_U_Doprava UNIQUE
  );

CREATE TABLE TypUhrady(
  Id int
    constraint TypUhrady_PK PRIMARY KEY,
  Uhrada nvarchar(30) NOT NULL
    constraint TypUhrady_U_Uhrada UNIQUE
  );

CREATE TABLE Objednavka(
  Id bigint
    constraint Objednavka_PK PRIMARY KEY,
  Stav char(10) NOT NULL
    constraint Objednavka_CHK_Stav
      check (Stav IN ('Kosik', 'Potvrzena', 'Odeslana', 'Stornovana')), -- v��tov� typ
  IdTypDopravy int
    constraint Objednavka_FK_TypDopravy REFERENCES TypDopravy(Id),
  IdTypUhrady int
    constraint Objednavka_FK_TypUhrady REFERENCES TypUhrady(Id),
  CasPotvrzeni datetime,
  Zaplacena bit NOT NULL DEFAULT 0,
  Jmeno nvarchar(30),
  Prijmeni nvarchar(30),
  Ulice nvarchar(30),
  Mesto nvarchar(30),
  Psc char(5)
    constraint Objednavka_CHK_Psc
      check (Psc like '[0-9][0-9][0-9][0-9][0-9]'),
  Email nvarchar(60)
    constraint Objednavka_CHK_Email
      check (Email like '%_@_%.__%'),
  constraint Objednavka_CHK_Vyplneni_Udaju -- objedn�vka je bu� ch�p�na jako ko��k, nebo u� byla n�kdy potvrzena a pak mus� b�t vypln�ny v�echny pot�ebn� �daje pro odesl�n� objedn�vky
    check (Stav = 'Kosik' OR (IdTypDopravy IS NOT NULL AND
                  IdTypUhrady IS NOT NULL AND
                  CasPotvrzeni IS NOT NULL AND
                  Jmeno IS NOT NULL AND
                  Prijmeni IS NOT NULL AND
                  Ulice IS NOT NULL AND
                  Mesto IS NOT NULL AND
                  Psc IS NOT NULL AND
                  Email IS NOT NULL)),
  constraint Objednavka_CHK_Zaplaceni -- objedn�vka ve stavu 'Kosik' mus� b�t nezaplacen�, ve stavu 'Odeslano' mus� b�t zaplacen�
    check ((Stav != 'Kosik' OR Zaplacena = 0) AND (Stav != 'Odeslana' OR Zaplacena = 1))
  );

CREATE TABLE PolozkaObjednavky(
  IdObjednavka bigint NOT NULL
    constraint PolozkaObjednavky_FK_Objednavka REFERENCES Objednavka(Id)
      on delete cascade,
  IdKniha int NOT NULL
    constraint PolozkaObjednavky_FK_Kniha REFERENCES Kniha(Id),
  Pocet int NOT NULL
    constraint PolozkaObjednavky_CHK_Pocet
      check (Pocet > 0),
  constraint PolozkaObjednavky_PK PRIMARY KEY (IdObjednavka, IdKniha)
  );