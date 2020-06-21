-- Internetový obchod s knihami

drop view Seznam_Kategorii_VW;
drop view Seznam_Autoru_VW;
drop view Seznam_Jazyku_VW;
drop view Seznam_Nakladatelstvi_VW;
drop view Vypis_Knih_Kategorie_VW;
drop view Vypis_Knih_Autor_VW;
drop view Vypis_Knih_Jazyk_VW;
drop view Vypis_Knih_Nakladatelstvi_VW;
drop view Detail_knihy_VW;
drop view Souvisejici_Knihy_VW;
drop view Potvrzene_Nezaplacene_Objednavky_VW;
drop view Potvrzene_Zaplacene_Objednavky_VW;
drop view Objednavka_VW;
drop view Dochazejici_Knihy_VW;
drop view Vypis_Polozek_Objednavky_VW;
drop view Prodejnost_Kniha_VW;
drop view Prodejnost_Autor_VW;
drop view Souvisejici_Souvisejicich_VW;
drop view Stornovane_Zaplacene_Objednavky_VW;

drop procedure ObjednanoUDodavatele;
drop procedure OznacObjednavkuNezaplacena;
drop procedure OznacObjednavkuOdeslana;
drop procedure OznacObjednavkuZaplacena;
drop procedure PotvrdObjednavku;
drop procedure PrisloOdDodavatele;
drop procedure StornujObjednavku;
drop procedure UpravPocetVKosiku;
drop procedure VytvorKosik;

drop function CenaObjednavky;

DROP TABLE PolozkaObjednavky;
DROP TABLE Objednavka;
DROP TABLE TypDopravy;
DROP TABLE TypUhrady;

DROP TABLE Souvisejici;
DROP TABLE Kniha;
DROP TABLE Kategorie;
DROP TABLE Nakladatelstvi;
DROP TABLE Jazyk;
DROP TABLE Autor;
