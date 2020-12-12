-- Zadání:
--   Vytvořte script (textový soubor), který obsahuje sekvenci SQL/JSON výrazů ilustrující práci s relačním a zároveň dokumentovým modelem
--   Definujte schéma pro 2 tabulky reprezentující entity různých typů
--     Definujte alespoň jeden sloupec pro každý z následujících datových typů: json, jsonb
--     Definujte referenční integritní omezení mezi tabulkami
--   Do každé tabulky vložte alespoň 5 řádků
--   Ilustujte, že umíte používat operátory a funkce pracující s datovými typy json/jsonb
--     Alespoň 1x použijte každý z následujících json/jsonb operátorů: ->, ->>, #>, #>>
--     Vhodně aplikujte zřetězení operátorů -> a ->>
--     Použijte alespoň 2 z následujících jsonb operátorů: @>, <@, ?, ?|, ?&
--   Vyjádřete alespoň 4 výrazy typu update
--     Nenahrazujte celý dokument, ale použijte k tomuto účelu vhodné operátory, např. ||, -, #-
--     Alespoň 1x použijte každou z následujících funkcí: jsonb_set, jsonb_insert
--     Alespoň 1x vykonejte update výraz nad vnořeným dokumentem
--     Alespoň 1x vykonejte update výraz nad polem
--   Vyjádřete alespoň 4 dotazy
--     Použijte klauzule WHERE, GROUP BY, HAVING a ORDER BY, a to každou z nich alespoň 1x
--     Alespoň 1x realizujte vnořený dotaz
--     Alespoň 2x reprezentujte výsledek celého dotazu v datovém typu jsonb, přičemž pokaždé k tomuto účelu použijte jinou funkci, např. to_json[b], array_to_json, row_to_json anebo json[b]_object
--     Alespoň 1x použijte přetypování (angl. type casting)
--     Alespoň 1x použijte agregační funkci
--     Vhodně používejte tzv. aliasing (AS)
--   Vytvořte alespoň jeden index nad sloupcem typu jsonb
-- Požadavky:
--   Pracujte pouze se svou databází a defaultním schématem
--     Jméno této databáze je shodné s Vaším loginem, např. m201_student
--     Defaultním schématem je "public"
--     Nevytvářejte ani nepoužívejte žádné další schéma uvnitř Vašeho odevzdávaného skriptu
-- Odevzdávané soubory:
--   script.sql: Textový soubor obsahující SQL/JSON výrazy
-- Vykonání scriptu:
--   Vykonejte následující shell přikaz: psql -d $DatabaseName -f $ScriptFile
--   $DatabaseName je jménem Vaší (existující) databáze, např. m201_student
--   $ScriptFile je jménem souboru s SQL/JSON výrazy, např. script.sql


DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS room;
DROP TABLE IF EXISTS reservation;

CREATE TABLE customer (
    id TEXT PRIMARY KEY,
    data JSON,
    reservations TEXT[]
);

CREATE TABLE room (
    id TEXT PRIMARY KEY,
    data JSON
);

CREATE TABLE reservation (
    id TEXT PRIMARY KEY,
    data JSONB,
    rooms TEXT[]
);

INSERT INTO customer (id,data,reservations) VALUES ('trojan','{ "name": { "first": "Ivan", "last": "Trojan" }, "year": 1964}', ARRAY[ 'samotari', 'medvidek', 'karamazovi' ]);
INSERT INTO customer (id,data,reservations) VALUES ('machacek','{"name": { "first": "Jiri", "last": "Machacek" }, "year": 1966}', ARRAY[ 'medvidek', 'vratnelahve', 'samotari' ]);
INSERT INTO customer (id,data,reservations) VALUES ('geislerova','{"name": { "first": "Anna", "last": "Geislerova" }, "year": 1976}');
INSERT INTO customer (id,data,reservations) VALUES ('vilhelmova','{"name": { "first": "Tatiana", "last": "Vilhelmova" }, "year": 1978}', ARRAY[ 'medvidek' ]);
INSERT INTO customer (id,data,reservations) VALUES ('menzel','{   "name": { "last": "Menzel", "first": "Jiri" }, "year": 1938}', ARRAY[ 'medvidek' ]);

INSERT INTO room (id,data)
    VALUES ('samotari','{ "title": { "cs": "Samotari", "en": "Loners" }, "year": 2000, "rating": 84, "length": 103, "actors": [ "trojan", "machacek", "schneiderova" ], "genres": [ "comedy", "drama" ], "country": [ "CZ", "SI" ] }');
INSERT INTO room (id,data)
    VALUES ('medvidek','{ "title" : "Medvidek", "year": 2007, "rating": 53, "length": 100, "director": { "first": "Jan", "last": "Hrebejk" }, "actors": [ "trojan", "machacek", "vilhelmova", "issova", "menzel" ], "genres": [ "comedy", "drama" ], "country": [ "CZ" ] }');
INSERT INTO room (id,data)
    VALUES ('vratnelahve','{ "title": { "cs": "Vratne lahve", "en": "Empties" }, "year": 2006, "rating":76, "length":99, "director": { "first": "Jan", "last": "Sverak" }, "actors": [ "sverak", "machacek", "schneiderova" ], "genres": "comedy", "country": "CZ" }');
INSERT INTO room (id,data)
    VALUES ('zelary','{ "title": "Zelary", "year": 2003, "rating":81, "length":142, "director": { "last": "Trojan", "first": "Ondrej" }, "actors": [ ], "genres": [ "romance", "drama" ], "country": [ "CZ", "SK", "AT" ] }');
INSERT INTO room (id,data)
    VALUES ('stesti','{ "title": "Stesti", "year": 2005, "rating": 72, "length": 100, "director": { "last": "Slama", "first": "Bohdan" }, "awards": [ { "type": "Czech Lion", "year": 2005 } ] }');

INSERT INTO reservation (id,data,rooms)
    VALUES ('samotari','{ "title": { "cs": "Samotari", "en": "Loners" }, "year": 2000, "rating": 84, "length": 103, "actors": [ "trojan", "machacek", "schneiderova" ], "genres": [ "comedy", "drama" ], "country": [ "CZ", "SI" ] }');
INSERT INTO reservation (id,data,rooms)
    VALUES ('medvidek','{ "title" : "Medvidek", "year": 2007, "rating": 53, "length": 100, "director": { "first": "Jan", "last": "Hrebejk" }, "actors": [ "trojan", "machacek", "vilhelmova", "issova", "menzel" ], "genres": [ "comedy", "drama" ], "country": [ "CZ" ] }');
INSERT INTO reservation (id,data,rooms)
    VALUES ('vratnelahve','{ "title": { "cs": "Vratne lahve", "en": "Empties" }, "year": 2006, "rating":76, "length":99, "director": { "first": "Jan", "last": "Sverak" }, "actors": [ "sverak", "machacek", "schneiderova" ], "genres": "comedy", "country": "CZ" }');
INSERT INTO reservation (id,data,rooms)
    VALUES ('zelary','{ "title": "Zelary", "year": 2003, "rating":81, "length":142, "director": { "last": "Trojan", "first": "Ondrej" }, "actors": [ ], "genres": [ "romance", "drama" ], "country": [ "CZ", "SK", "AT" ] }');
INSERT INTO reservation (id,data,rooms)
    VALUES ('stesti','{ "title": "Stesti", "year": 2005, "rating": 72, "length": 100, "director": { "last": "Slama", "first": "Bohdan" }, "awards": [ { "type": "Czech Lion", "year": 2005 } ] }');