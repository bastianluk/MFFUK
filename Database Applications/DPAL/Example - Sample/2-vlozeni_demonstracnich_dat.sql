-- Internetový obchod s knihami

/*
delete from Souvisejici;
delete from PolozkaObjednavky;
delete from Objednavka;
delete from Kniha;
delete from Autor;
delete from Jazyk;
delete from Nakladatelstvi;
delete from Kategorie;
delete from TypDopravy;
delete from TypUhrady;
*/

insert into Autor
  (Id, Jmeno, Prijmeni)
  values
  (1, 'Karel Havlíček', 'Borovský'),
  (2, 'Karel Jaromír', 'Erben'),
  (3, 'Ernest', 'Hemingway'),
  (4, 'Stieg', 'Larsson'),
  (5, NULL, 'Eucleides'),
  (6, 'Giovanni', 'Boccaccio'),
  (7, 'Hakan', 'Nesser'),
  (8, 'Ake', 'Hodell'),
  (9, 'Veronica', 'Roth'),
  (10, 'Suzanne', 'Collins');

insert into Jazyk
  (Id, Nazev)
  values
  (1, 'Čeština'),
  (2, 'Angličtina'),
  (3, 'Švédština'),
  (4, 'Řečtina'),
  (5, 'Italština');

insert into Kategorie
  (Id, IdNadrazenaKategorie, Nazev, Popis)
  values
  (1, NULL, 'Romány', 'Román je prozaický epický literární žánr, smyšlené vyprávění. Vývoj románu je dlouhý, což se výrazně projevilo na proměnlivosti jeho struktury.'),
  (2, NULL, 'Fantasy', 'Fantasy je umělecký žánr, používaný především v literatuře a filmu, ale i ve výtvarném umění, založený především na užití magie či jiných nadpřirozených prvků.'),
  (3, NULL, 'Horory', 'Horor (z lat. horror – hrůza, zděšení) je umělecký žánr, jehož cílem je u čtenáře nebo diváka vyvolat pocit strachu a děsu.'),
  (4, NULL, 'Zdraví', 'Nechoďte k doktorovi, uzdravte se sami podle našich úžasných knih.'),

  (5, 1, 'Dobrodružné', 'Dobrodružné knihy'),
  (6, 1, 'Historické', 'Historické knihy'),
  (7, 1, 'Válečné', 'Knihy o válce'),
  (8, 1, 'Biografie', 'Biografické knihy'),
  (9, 1, 'Komedie', 'Pro zasmání'),

  (10, 2, 'Fantasy', 'Fantasy knihy'),
  (11, 2, 'Sci-fi', 'Vědecko-fantastické knihy'),

  (12, 3, 'Horory', 'Horory'),
  (13, 3, 'Thrillery', NULL),

  (14, 4, 'Životní styl', 'Knihy o zdraví'),
  (15, 4, 'Zdravotní problémy', 'Řešení zdravotních problémů'),
  (16, 4, 'Diety', 'Diety a hubnutí'),

  (17, NULL, 'Výprodej', 'Knihy ve výprodeji.');


insert into Nakladatelstvi
  (Id, Nazev, Adresa)
  values
  (1, 'Argo', 'Celetná 25, 10200 Praha 1'),
  (2, 'Host', 'Veletržní 12A, 16002 Brno'),
  (3, 'Fragment', 'Horní 100, 70030 Ostrava'),
  (4, 'Academia', 'Bílinská 18, 19000 Praha 9'),
  (5, 'Ikarus', 'Bezděkov 229, 45600, Česká Třebová'),
  (6, 'Mafra', 'Horymírova 14, 10200, Praha 1'),
  (7, 'Taxus', 'V Potočkách 16, 14300 Praha 12');

insert into Kniha
  (Id, IdKategorie, IdAutor, IdNakladatelstvi, IdJazyk, Nazev, Popis, RokVydani, PocetStran, ISBN, PocetNaSkladu, PocetObjednano, Cena)
  values
  (1, 5, 2, 1, 1, 'Kytice', 'Kytice, původně vydaná pod názvem Kytice z pověstí národních, je jedno ze stěžejních děl Karla Jaromíra Erbena.', NULL, 157, '978-3-16-148410-0', 5, 2, 150.00),
  (2, 1, 4, 2, 3, 'Muži kteří nenávidí ženy', 'První díl severské detektivní trilogie Millénium.', 2007, 189, '987-4-15-25732-1', 1, 7935, 398.90),
  (3, 1, 4, 2, 3, 'Dívka, která si hrála s ohněm', 'Druhý díl trilogie Millénium.', 2008, NULL, '965-4-65-45783-3', 4, 2133, 354.00),
  (4, 1, 4, 2, 3, 'Dívka,která kopla do vosího hnízda', 'Třetí díl trilogie Millénium.', 2009, 215, '646-5-65-65432-1', 3, 3224, 313.00),
  (5, 11, 9, 6, 2, 'Divergence', 'Rozvrácená země spojena pomocí systému frakcí.', 2011, 345, '845-3-46-65178-2', 9, 112, 355.20),
  (6, 11, 9, 6, 2, 'Rezistence', NULL, 2012, 355, '924-2-34-58146-6', 12, 145, 320.00),
  (7, 11, 9, 6, 2, 'Aliance', 'Poslední díl trilogie britské spisovatelky.', 2012, 395, '958-1-35-74125-4', 6, 150, 370.00),
  (8, 13, 10, 6, 2, 'Hunger Games: Aréna smrti', 'První díl dobrodružství Katniss Everdeenové', 2008, 277, '465-5-35-25874-6', 46, 32, 333.00),
  (9, 13, 10, 6, 2, 'Hunger Games: Vražedná pomsta', 'Druhý díl dobrodružství K. Everdeenové.', 2013, 365, '684-5-79-78425-6', 12, 55, 350.00),
  (10, 13, 10, 6, 2, 'HungerGames: Síla vzdoru', 'Třetí díl strhujícího příběhu.', 2014, 450, '921-5-34578-3', 1, 3, 385.00),
  (11, 17, 5, 4, 4, 'Základy', 'Pro matematiky', NULL, 201, '453-4-15-72156-4', 1, 0, 50);

insert into TypDopravy
  (Id, Doprava)
  values
  (1, 'Osobní odběr'),
  (2, 'Na poštu'),
  (3, 'ulozenka.cz');

insert into TypUhrady
  (Id, Uhrada)
  values
  (1, 'Bankovním převodem'),
  (2, 'Kartou');


insert into Objednavka
  (Id, Stav, IdTypDopravy, IdTypUhrady, CasPotvrzeni, Zaplacena, Jmeno, Prijmeni, Ulice, Mesto, Psc, Email)
  values
  (1, 'Potvrzena', 1, 1, '2015-06-28 13:25:18', 0, 'Adam', 'Sedlák', 'Smrková 27', 'Jičíněves', '16700', 'adam.sedlak@seznam.cz'),
  (2, 'Odeslana', 2, 1, '2015-02-15 20:02:22', 1, 'David', 'Novák', 'Ulice 1', 'Praha', '12300', 'kfjl@fjslkafj.kd'),
  (3, 'Potvrzena', 1, 2, '2015-05-20 11:01:11', 0, 'Daniel', 'Tichý', 'K otočce 15', 'Brno', '43401', 'tichy@microsoft.com'),
  (4, 'Stornovana', 2, 2, '2015-06-15 15:12:55', 1, 'Tereza', 'Nerozhodná', 'neřeknu', ':P', '00000', 'terezanerozhodna@centrum.cz'),
  (5, 'Kosik', NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL),
  (6, 'Odeslana', 3, 2, '2015-03-29 23:10:11', 1, 'Jan', 'Kupující', 'Zelená 6', 'Poděbrady', '24100', 'kupujicijan@gmail.com'),
  (8, 'Kosik', NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL),
  (33, 'Potvrzena', 1, 2, '2015-05-21 11:01:11', 1, 'Daniel', 'Tichý', 'K otočce 152', 'Brno', '43401', 'tichy@microsoft.com'),
  (34, 'Potvrzena', 2, 2, '2015-04-20 12:21:51', 1, 'Daniel', 'Tichý', 'K otočce 152', 'Brno', '43401', 'tichy@microsoft.com'),
  (35, 'Potvrzena', 1, 1, '2015-01-11 18:11:24', 1, 'Daniel', 'Tichý', 'K otočce 152', 'Brno', '43401', 'tichy@microsoft.com'),
  (1001, 'Kosik', NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL),
  (1002, 'Kosik', NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL),
  (1003, 'Potvrzena', 1, 1, '2015-01-03 00:00:00', 0, '3', '', '', '', '11111', '11@111.111'),
  (1004, 'Potvrzena', 1, 1, '2015-01-04 00:00:00', 1, '4', '', '', '', '11111', '11@111.111'),
  (1005, 'Odeslana', 1, 1, '2015-01-05 00:00:00', 1, '5', '', '', '', '11111', '11@111.111'),
  (1006, 'Odeslana', 1, 1, '2015-01-06 00:00:00', 1, '6', '', '', '', '11111', '11@111.111'),
  (1007, 'Stornovana', 1, 1, '2015-01-07 00:00:00', 0, '7', '', '', '', '11111', '11@111.111'),
  (1008, 'Stornovana', 1, 1, '2015-01-08 00:00:00', 1, '8', '', '', '', '11111', '11@111.111');

insert into PolozkaObjednavky
  (IdObjednavka, IdKniha, Pocet)
  values
  (1, 1, 1),
  (1, 6, 2),
  (1, 7, 1),
  (2, 8, 1),
  (2, 9, 1),
  (2, 10, 1),
  (3, 2, 4),
  (3, 3, 4),
  (3, 4, 4),
  (3, 7, 1),
  (4, 5, 1),
  (5, 3, 3),
  (6, 4, 7),
  (6, 5, 1),
  (6, 8, 3),
  (6, 9, 11),
  (6, 10, 2),
  (33, 10, 1),
  (34, 10, 1),
  (35, 10, 1),
  (1001, 10, 1),
  (1003, 10, 1),
  (1004, 10, 1),
  (1005, 10, 1),
  (1006, 10, 1),
  (1007, 10, 1),
  (1008, 10, 1);

insert into Souvisejici
  (IdKniha, IdSouvisejiciKniha)
  values
  (1, 2),
  (1, 3),
  (1, 9),
  (2, 3),
  (2, 4),
  (3, 2),
  (3, 4),
  (4, 2),
  (4, 3),
  (5, 6),
  (6, 5),
  (6, 7),
  (7, 5),
  (7, 6),
  (8, 9),
  (8, 10),
  (9, 8),
  (9, 10),
  (10, 8),
  (10, 9);
