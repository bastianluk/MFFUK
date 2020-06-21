-- Internetový obchod s knihami

update statistics Autor;
update statistics Jazyk;
update statistics Kategorie;
update statistics Kniha;
update statistics Nakladatelstvi;
update statistics Objednavka;
update statistics PolozkaObjednavky;
update statistics Souvisejici;
update statistics TypDopravy;
update statistics TypUhrady;

create statistics Kategorie_Nazev on Kategorie(Nazev) with fullscan;
create statistics Kniha_Nazev on Kniha(Nazev) with fullscan;
create statistics Kniha_Cena on Kniha(Cena) with fullscan;