-- Internetov� obchod s knihami

-- seznam kategorii s podkategoriemi pro vypis v menu. Obsahuje v�dy dvojici (hlavn� kategorie, podkategorie) a obsahuje i hlavn� kategorie bez podkategori�
create view Seznam_Kategorii_VW as
  select HlavniKategorie.Id Hlavni_Id,
      HlavniKategorie.Nazev Hlavni_Nazev,
      Podkategorie.Id Podkategorie_Id,
      Podkategorie.Nazev Podkategorie_Nazev
  from Kategorie HlavniKategorie
    left join Kategorie Podkategorie on HlavniKategorie.Id = Podkategorie.IdNadrazenaKategorie
  where HlavniKategorie.IdNadrazenaKategorie IS NULL
go

-- v�pis autor� s po�tem jejich knih
create view Seznam_Autoru_VW as
  select Autor.*, (select count(*) from Kniha where Kniha.IdAutor = Autor.Id) Pocet
    from Autor
go

-- v�pis nakladatelstv� s po�tem jejich knih
create view Seznam_Nakladatelstvi_VW as
  select Nakladatelstvi.*, (select count(*) from Kniha where Kniha.IdNakladatelstvi = Nakladatelstvi.Id) Pocet
    from Nakladatelstvi
go

-- v�pis jazyk� s po�tem p��slu�n�ch knih
create view Seznam_Jazyku_VW as
  select Jazyk.*, (select count(*) from Kniha where Kniha.IdJazyk = Jazyk.Id) Pocet
    from Jazyk
go

-- knihy p�ipraven� pro v�b�r kategorie
create view Vypis_Knih_Kategorie_VW as
  select Autor.Jmeno, Autor.Prijmeni, Kniha.IdKategorie, Kniha.Nazev, Kniha.Cena
  from Kniha
    join Autor on Kniha.IdAutor = Autor.Id
go

-- knihy p�ipraven� pro v�b�r autora
create view Vypis_Knih_Autor_VW as
  select Kniha.IdAutor, Kniha.Nazev, Kniha.Cena
  from Kniha
go

-- knihy p�ipraven� pro v�b�r nakladatelstv�
create view Vypis_Knih_Nakladatelstvi_VW as
  select Autor.Jmeno, Autor.Prijmeni, Kniha.IdNakladatelstvi, Kniha.Nazev, Kniha.Cena
  from Kniha
    join Autor on Kniha.IdAutor = Autor.Id
go

-- knihy p�ipraven� pro v�b�r jazyka
create view Vypis_Knih_Jazyk_VW as
  select Autor.Jmeno, Autor.Prijmeni, Kniha.IdJazyk, Kniha.Nazev, Kniha.Cena
  from Kniha
    join Autor on Kniha.IdAutor = Autor.Id
go

-- Detail knihy, pro v�pis str�nky detailu knihy
create view Detail_Knihy_VW as
  select Autor.Jmeno,
      Autor.Prijmeni,
      Jazyk.Nazev Jazyk,
      Kategorie.Nazev Kategorie,
      HlavniKategorie.Nazev HlavniKategorie,
      Nakladatelstvi.Nazev Nakladatelstvi,
      Kniha.*
  from Kniha
    join Autor on Kniha.IdAutor = Autor.Id
    join Jazyk on Kniha.IdJazyk = Jazyk.Id
    join Kategorie on Kniha.IdKategorie = Kategorie.Id
    left join Kategorie HlavniKategorie on Kategorie.IdNadrazenaKategorie = HlavniKategorie.Id
    join Nakladatelstvi on Kniha.IdNakladatelstvi = Nakladatelstvi.Id
go

-- pro v�pis souvisej�c�ch knih v detailu knihy
create view Souvisejici_Knihy_VW as
  select Souvisejici.IdKniha,
      Autor.Jmeno,
      Autor.Prijmeni,
      Kniha.Id IdSouvisejici,
      Kniha.Nazev,
      Kniha.Cena
  from Souvisejici
    join Kniha on Souvisejici.IdSouvisejiciKniha = Kniha.Id
    join Autor on Kniha.IdAutor = Autor.Id
go

-- z�kladn� informace o objedn�vce bez polo�ek objedn�vky
create view Objednavka_VW as
  select TypDopravy.Doprava,
      TypUhrady.Uhrada,
      dbo.CenaObjednavky(Objednavka.Id) CelkovaCena,
      Objednavka.*
  from Objednavka
    left join TypDopravy on Objednavka.IdTypDopravy = TypDopravy.Id
    left join TypUhrady on Objednavka.IdTypUhrady = TypUhrady.Id
go

-- v�pis v�ech polo�ek objedn�vky s podrobnostmi dosta�uj�c�mi pro v�pis pro u�ivatele i pro zam�stnance
create view Vypis_Polozek_Objednavky_VW as
  select PolozkaObjednavky.IdObjednavka,
      Kniha.Id IdKniha,
      Kniha.Nazev,
      Autor.Jmeno,
      Autor.Prijmeni,
      Kniha.Cena,
      PolozkaObjednavky.Pocet PocetVObjednavce,
      Kniha.PocetNaSkladu,
      Kniha.ISBN
  from PolozkaObjednavky
    join Kniha on PolozkaObjednavky.IdKniha = Kniha.Id
    join Autor on Kniha.IdAutor = Autor.Id
go

create view Potvrzene_Nezaplacene_Objednavky_VW as
  select TypDopravy.Doprava, TypUhrady.Uhrada, Objednavka.*
  from Objednavka
    join TypDopravy on Objednavka.IdTypDopravy = TypDopravy.Id
    join TypUhrady on Objednavka.IdTypUhrady = TypUhrady.Id
  where Stav='Potvrzena' AND Zaplacena = 0
go

create view Potvrzene_Zaplacene_Objednavky_VW as
  select TypDopravy.Doprava, TypUhrady.Uhrada, Objednavka.*
  from Objednavka
    join TypDopravy on Objednavka.IdTypDopravy = TypDopravy.Id
    join TypUhrady on Objednavka.IdTypUhrady = TypUhrady.Id
  where Stav='Potvrzena' AND Zaplacena = 1
go

create view Stornovane_Zaplacene_Objednavky_VW as
  select TypDopravy.Doprava, TypUhrady.Uhrada, Objednavka.*
  from Objednavka
    join TypDopravy on Objednavka.IdTypDopravy = TypDopravy.Id
    join TypUhrady on Objednavka.IdTypUhrady = TypUhrady.Id
  where Stav='Stornovana' AND Zaplacena = 1
go

-- seznam knih, kter�ch je m�lo (na sklad� + objednan� < 5)
create view Dochazejici_Knihy_VW as
  select Kniha.Id,
      Kniha.ISBN,
      Kniha.Nazev,
      Autor.Jmeno,
      Autor.Prijmeni,
      Nakladatelstvi.Nazev Nakladatelstvi,
      Kniha.PocetNaSkladu,
      Kniha.PocetObjednano,
      Kniha.Cena,
      (Kniha.PocetNaSkladu + Kniha.PocetObjednano) VyhledoveDostupne
  from Kniha
    join Autor on Kniha.IdAutor = Autor.Id
    join Nakladatelstvi on Kniha.IdNakladatelstvi = Nakladatelstvi.Id
  where PocetNaSkladu + PocetObjednano < 5
go

-- tabulka knih a ke ka�d� po�et prodan�ch kus�
create view Prodejnost_Kniha_VW as
  with Prodejnost (Id, ProdanoKusu) as
    (select IdKniha, sum(Pocet)
      from PolozkaObjednavky
        join Objednavka on PolozkaObjednavky.IdObjednavka = Objednavka.Id
      where Objednavka.Stav = 'Odeslana'
      group by IdKniha
    )
  select Kniha.Id,
      Kniha.ISBN,
      Kniha.Nazev,
      Autor.Id IdAutor,
      Autor.Jmeno,
      Autor.Prijmeni,
      Kniha.Cena,
      Prodejnost.ProdanoKusu
  from Prodejnost
    join Kniha on Prodejnost.Id = Kniha.Id
    join Autor on Kniha.IdAutor = Autor.Id
go

-- tabulka autor� a ke ka�d�mu celkov� po�et prodan�ch kus� jeho knih
create view Prodejnost_Autor_VW as
  with Prodejnost (Id, ProdanoKusu) as
    (select IdAutor, sum(PolozkaObjednavky.Pocet)
      from PolozkaObjednavky
        join Objednavka on PolozkaObjednavky.IdObjednavka = Objednavka.Id
        join Kniha on PolozkaObjednavky.IdKniha = Kniha.Id
      where Objednavka.Stav = 'Odeslana'
      group by IdAutor
    )
  select Autor.Id,
      Autor.Jmeno,
      Autor.Prijmeni,
      Prodejnost.ProdanoKusu
  from Prodejnost
    join Autor on Prodejnost.Id = Autor.Id
go

-- tabulka knih a k nim souvisej�c�ch knih "2. ��du", kter� nejsou p��mo souvisej�c� (pro hled�n� nov�ch souvisej�c�ch ke knize)
create view Souvisejici_Souvisejicich_VW as
  with Souvisejici2 (Id1, Id2) as
    (select distinct S1.IdKniha, S2.IdSouvisejiciKniha from Souvisejici S1 join Souvisejici S2 on S1.IdSouvisejiciKniha = S2.IdKniha where S1.IdKniha != S2.IdSouvisejiciKniha
      except select * from Souvisejici)
  select Id1 IdPuvodniKniha, Kniha.*
  from Souvisejici2
    join Kniha on Souvisejici2.Id2 = Kniha.Id
go