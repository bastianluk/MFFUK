-- Internetov� obchod s knihami

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

-- index pro �azen� podkategori� podle n�zvu
create index Kategorie_NadrazenaKategorie_Nazev ON Kategorie(IdNadrazenaKategorie, Nazev);

-- �asto se vyb�raj� objedn�vky v ur�it�m stavu a zji��uje se u nich, jestli jsou zaplacen� a budou se �adit podle �asu potvrzen�
CREATE INDEX Objednavka_Stav_Zaplacena_Cas ON Objednavka(Stav, Zaplacena, CasPotvrzeni);

-- �asto se budou �adit v�pisy autor�, nakladatelstv� a jazyk�
create index Autor_Prijmeni_Jmeno on Autor(Prijmeni, Jmeno);
create index Nakladatelstvi_Nazev on Nakladatelstvi(Nazev);
create index Jazyk_Nazev on Jazyk(Nazev);

-- indexy pro �azen� knih ve v�pisu (tabulka knih se p��li� �asto nem�n�, naopak v�pisy knih v ur�it�m po�ad� prov�d� prakticky ka�d� n�v�t�vn�k obchodu)
CREATE INDEX Kniha_Kategorie_Nazev ON Kniha(IdKategorie, Nazev);
CREATE INDEX Kniha_Kategorie_Cena ON Kniha(IdKategorie, Cena);
CREATE INDEX Kniha_Autor_Nazev ON Kniha(IdAutor, Nazev);
CREATE INDEX Kniha_Autor_Cena ON Kniha(IdAutor, Cena);
CREATE INDEX Kniha_Nakladatelstvi_Nazev ON Kniha(IdNakladatelstvi, Nazev);
CREATE INDEX Kniha_Nakladatelstvi_Cena ON Kniha(IdNakladatelstvi, Cena);
CREATE INDEX Kniha_Jazyk_Nazev ON Kniha(IdJazyk, Nazev);
CREATE INDEX Kniha_Jazyk_Cena ON Kniha(IdJazyk, Cena);
