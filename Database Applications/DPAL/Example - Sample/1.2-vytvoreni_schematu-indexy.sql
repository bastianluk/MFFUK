-- Internetovı obchod s knihami

-- indexy pro cizi klice

CREATE INDEX Kategorie_IdNadrazenaKategorie ON Kategorie(IdNadrazenaKategorie);
CREATE INDEX Kniha_IdKategorie ON Kniha(IdKategorie);
CREATE INDEX Kniha_IdAutor ON Kniha(IdAutor);
CREATE INDEX Kniha_IdNakladatelstvi ON Kniha(IdNakladatelstvi);
CREATE INDEX Kniha_IdJazyk ON Kniha(IdJazyk);
CREATE INDEX Souvisejici_IdSouvisejiciKniha ON Souvisejici(IdSouvisejiciKniha);
CREATE INDEX Objednavka_IdTypDopravy ON Objednavka(IdTypDopravy);
CREATE INDEX Objednavka_IdTypUhrady ON Objednavka(IdTypUhrady);
CREATE INDEX PolozkaObjednavky_IdKniha ON PolozkaObjednavky(IdKniha);

-- index pro øazení podkategorií podle názvu
create index Kategorie_NadrazenaKategorie_Nazev ON Kategorie(IdNadrazenaKategorie, Nazev);

-- èasto se vybírají objednávky v urèitém stavu a zjišuje se u nich, jestli jsou zaplacené a budou se øadit podle èasu potvrzení
CREATE INDEX Objednavka_Stav_Zaplacena_Cas ON Objednavka(Stav, Zaplacena, CasPotvrzeni);

-- èasto se budou øadit vıpisy autorù, nakladatelství a jazykù
create index Autor_Prijmeni_Jmeno on Autor(Prijmeni, Jmeno);
create index Nakladatelstvi_Nazev on Nakladatelstvi(Nazev);
create index Jazyk_Nazev on Jazyk(Nazev);

-- indexy pro øazení knih ve vıpisu (tabulka knih se pøíliš èasto nemìní, naopak vıpisy knih v urèitém poøadí provádí prakticky kadı návštìvník obchodu)
CREATE INDEX Kniha_Kategorie_Nazev ON Kniha(IdKategorie, Nazev);
CREATE INDEX Kniha_Kategorie_Cena ON Kniha(IdKategorie, Cena);
CREATE INDEX Kniha_Autor_Nazev ON Kniha(IdAutor, Nazev);
CREATE INDEX Kniha_Autor_Cena ON Kniha(IdAutor, Cena);
CREATE INDEX Kniha_Nakladatelstvi_Nazev ON Kniha(IdNakladatelstvi, Nazev);
CREATE INDEX Kniha_Nakladatelstvi_Cena ON Kniha(IdNakladatelstvi, Cena);
CREATE INDEX Kniha_Jazyk_Nazev ON Kniha(IdJazyk, Nazev);
CREATE INDEX Kniha_Jazyk_Cena ON Kniha(IdJazyk, Cena);
