-- Internetový obchod s knihami

-- Zadání úlohy a vytvoøení tabulek


/*
Internetový obchod s knihami
----------------------------

Internetový obchod prodává pouze knihy. Každá kniha má autora (ten má jméno a pøíjmení, jméno nemusí být vyplnìno), vydalo ji nakladatelství (to má název a adresu), je napsaná v nìjakém jazyku, je zaøazena do nìjaké jedné kategorie knih, má název, ISBN, evidujeme u ní poèet knih na skladu, poèet objednaných knih od dodavatele a cenu. Volitelnì u knihy je popis, rok vydání a poèet stran. Pro každou knihu evidujeme, které knihy jsou k ní související (relace nemusí být symetrická, obvykle pro jednu knihu budou maximálnì jednotky souvisejících knih, desítky velmi ojedinìle).

Knihy jsou v obchodì uspoøádány v dvouúrovòových kategoriích - jsou hlavní kategorie a každá mùže mít podkategorie. Knihy mohou náležet jak do hlavní kategorie (to je využito, pokud kniha zøejmì patøí do dané hlavní kategorie, ale neexistuje žádná vhodná podkategorie), tak do podkategorie. Každá kategorie má název a volitelnì popis. Názvy kategorií jsou unikátní pouze v dané kategorii (tj. nemohou se jmenovat stejnì dvì hlavní kategorie a nemohou se jmenovat stejnì dvì podkategorie v jedné hlavní kategorii).


Správa objednávky
-----------------

Každá objednávka má pøidìlený náhodný tajný unikátní kód. Ten se napø. ukládá v cookies, aby uživatel mìl uchovaný stav košíku mezi návštìvami. Po potvrzení objednávky slouží kód ke sledování stavu objednávky (zákazníkovi je na e-mail odeslán odkaz pro sledování objednávky, kde se mùže podívat na obsah objednávky a zda je objednávka evidována jako zaplacená a jako odeslaná a kde mùže objednávku ještì stornovat, pokud nebyla odeslaná).

Výchozí stav je, že uživatel nemá vytvoøenu žádnou objednávku se stavem "Kosik". Ve chvíli, kdy pøidá položku do košíku (v libovolném množství, stav skladu se bude kontrolovat až pøi potvrzování objednávky), se mu vytvoøí objednávka ve stavu "Kosik" (pøi odebrání poslední položky z košíku se taková objednávka už neodstraòuje). Ve chvíli, kdy uživatel bude chtít vybrané položky v košíku koupit, zadá všechny potøebné údaje (typ dopravy, typ platby, jméno, pøíjmení, ulici, mìsto, PSÈ, e-mailovou adresu) a pokud jsou všechny vybrané knihy dostupné, tak se stav objednávky zmìní na "Potvrzena" a knihy se odeberou ze skladových zásob. Položky objednávky lze editovat pouze ve stavu "Kosik".

Pokud zákazník chce objednávku stornovat pøed odesláním, mùže pøevést stav objednávky z "Potvrzena" na "Stornovana", knihy se zpátky pøiètou do skladových zásob. Po odeslání knih zákazníkovi (obchod pøijímá pouze platby pøedem, takže objednávka se odešle teprve po zaplacení) se stav objednávky zmìní na "Odeslana".


Zobrazované stránky pro návštìvníky
-----------------------------------

- menu - výpis všech kategorií a podkagegorií, zobrazuje se vždy, každá položka má pouze název

- výpis autorù - zobrazeni všichni autoøi s informací o poètu rùzných knih od každého autora, øazení podle pøíjmení a jména autora
- výpis nakladatelství - zobrazeny všechny názvy (a v kontextové nápovìdì adresy) nakladatelství s informací o poètu rùzných knih od každého nakladatelství, øazení podle názvu nakladatelství
- výpis jazykù - zobrazeny všechny jazyky s informací o poètu rùzných knih od každého jazyka, øazení podle názvu jazyka
- výpis knih (pro kategorii, autora, nakladatelství nebo jazyk) - u knih zobrazen název, autor (kromì zobrazení podle autora) a cena, øadit je možné podle názvu knihy nebo ceny
- zobrazení jedné knihy - zobrazeny všechny relevantní dostupné informace o konkrétní knize, zobrazeny všechny související knihy (pouze název, autor a cena, øazení podle názvu)
- zobrazení košíku - výpis všech položek v košíku, u každé položky se zobrazuje název knihy, autor, poèet kusù, cena a informace, zda je dostateèný poèet kusù na skladì
- sledování objednávky - výpis všech položek v košíku, u každé položky se zobrazuje název knihy, autor, poèet kusù a cena

Administrace webu
-----------------

Bìžné zobrazení pro návštìvníky bude doplnìno o administraèní odkazy (odebrání položky, editace položky, pøidání položky). Navíc budou nìkteré stránky administrace.

Zamìstnanci obchodu budou moct pøidávat, upravovat, odstraòovat
  - autory
  - nakladatelství
  - jazyky
  - kategorie
  - knihy
  - související knihy
Tyto editace se budou provádìt spíše ojedinìle (vydání nové knihy, oprava chybnì zadaného údaje, úprava souvisejících knih, ...), bude proto možné provádìt úpravy libovolných dostupných údajù entit.

Pøi bìžném provozu obchodu budou zamìstnanci využívat hlavnì tyto zobrazení a možnosti:

(vyøizování objednávek)
- Zobrazit seznam potvrzených nezaplacených objednávek
  - Možnost oznaèit objednávku jako zaplacenou
  - Možnost zmìnit stav na "Stornovana" (napø. uživatel psal e-mail a ztratil už kód objednávky)
- Zobrazit seznam potvrzených zaplacených objednávek
  - Možnot zmìnit stav na "Odeslana" (po zabalení a odeslání knih)
  - Možnost zmìnit stav na "Stornovana" (napø. uživatel psal e-mail a ztratil už kód objednávky, zašlou se peníze zpìt)
- Zobrazit seznam stornovaných zaplacených objednávek
  - Možnot zmìnit objednávku na nezaplacenou (po vrácení penìz zákazníkovi)
- Zobrazit detail objednávky - položky objednávky s ISBN (zobrazení podobné zobrazení košíku pro zákazníka, slouží jako seznam knih, které je potøeba najít ve skladu, zabalit a odeslat, ve skladu jsou knihy seøazené podle ISBN)

(objednávání knih u dodavatele a naskladòování)
- Zobrazit knihy s malým poètem výhledovì dostupných knih (na skladu + objednáno < 5), seøazené vzestupnì podle poètu výhledovì dostupných knih, dùležitý je ISBN, podle kterého se bude objednávat u dodavatele
- Vyhledat knihu podle ISBN
- Možnost u knihy zvìtšit poèet objednaných knih (když zamìstnanec objednal knihu u dodavatele)
- Možnost u knihy pøesunout urèitý poèet z objednaných do skladových (když knihy pøijdou od dodavatele)

Obchod bude také umožòovat sledovat urèité statistiky a zajímavé údaje:
- Výpis knih s poètem prodaných kusù (pro nalezení nejprodávanìjších a neprodávaných knih)
- Výpis autorù s poètem prodaných kusù
- Zobrazení souvisejících knih souvisejících knih, které nejsou pøímo související (to se hodí v pøípadì, že máme knihu a k ní málo souvisejících knih, je pravdìpodobné, že související knihy souvisejících knih budou souviset i s pùvodní knihou)
- Seznam potvrzených, ale nezaplacených objednávek, které jsou starší než mìsíc (potvrzená objednávka blokuje knihy, staré nezaplacené objednávky se mùžou stornovat)
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
      check (Stav IN ('Kosik', 'Potvrzena', 'Odeslana', 'Stornovana')), -- výètový typ
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
  constraint Objednavka_CHK_Vyplneni_Udaju -- objednávka je buï chápána jako košík, nebo už byla nìkdy potvrzena a pak musí být vyplnìny všechny potøebné údaje pro odeslání objednávky
    check (Stav = 'Kosik' OR (IdTypDopravy IS NOT NULL AND
                  IdTypUhrady IS NOT NULL AND
                  CasPotvrzeni IS NOT NULL AND
                  Jmeno IS NOT NULL AND
                  Prijmeni IS NOT NULL AND
                  Ulice IS NOT NULL AND
                  Mesto IS NOT NULL AND
                  Psc IS NOT NULL AND
                  Email IS NOT NULL)),
  constraint Objednavka_CHK_Zaplaceni -- objednávka ve stavu 'Kosik' musí být nezaplacená, ve stavu 'Odeslano' musí být zaplacená
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