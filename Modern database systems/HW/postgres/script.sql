-- bastian - theme - hotel

-- Zadání:
--   Vytvořte script (textový soubor), který obsahuje sekvenci SQL/JSON výrazů ilustrující práci s relačním a zároveň dokumentovým modelem
--     Definujte referenční integritní omezení mezi tabulkami
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
-- Požadavky:
--   Pracujte pouze se svou databází a defaultním schématem
--     Jméno této databáze je shodné s Vaším loginem, např. m201_student
--     Defaultním schématem je "public"
-- Vykonání scriptu:
--   Vykonejte následující shell přikaz: psql -d $Database"name" -f $ScriptFile
--   $Database"name" je jménem Vaší (existující) databáze, např. m201_student
--   $ScriptFile je jménem souboru s SQL/JSON výrazy, např. script.sql

--
-- PRE exec CLEANUP
DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS room;
DROP TABLE IF EXISTS reservation;


--
-- CREATE TABLES
CREATE TABLE customer (
    id TEXT PRIMARY KEY,
    data JSON,
    reservations TEXT[]
);
CREATE TABLE room (
    id TEXT PRIMARY KEY,
    data JSONB
);
CREATE TABLE reservation (
    id SERIAL PRIMARY KEY,
    data JSON,
    rooms TEXT[]
);


--
-- INSERT
INSERT INTO customer (id,data,reservations)
    VALUES ('gyllenhaalj','{ "name": { "first" : "Jake", "last" : "Gyllenhaal" }, "country": "USA", "email": "gyllenhaalj@gmail.com", "year": 1980 }', ARRAY[ '1' ]);
INSERT INTO customer (id,data,reservations)
    VALUES ('gyllenhaalm','{ "name": { "first" : "Maggie", "last" : "Gyllenhaal" }, "country": "USA", "email": "gyllenhaalm@gmail.com", "year": 1977 }', ARRAY[ '2' ]);
INSERT INTO customer (id,data,reservations)
    VALUES ('cage','{ "name": { "first" : "Nicolas", "last" : "Cage" }, "country": "USA", "email": "cage@goodactor.not", "year": 1964 }', ARRAY[ '3' ]);
INSERT INTO customer (id,data,reservations)
    VALUES ('cavill','{ "name":{ "first" : "Henry", "last" : "Cavill" }, "country": "GBR", "email": "cavill@thewitcher.pl", "year": 1983 }', ARRAY[ '5' ]);
INSERT INTO customer (id,data,reservations)
    VALUES ('sverak','{ "name": { "first" : "Zdenek", "last" : "Sverak" }, "country": "CZE", "email": "sverak@atlas.cz", "year": 1936 }', ARRAY[ '4' ]);

INSERT INTO room (id,data)
    VALUES ('single1','{ "number": "201", "category": "single", "capacity": 1, "features" : [ "workplace" ] }');
INSERT INTO room (id,data)
    VALUES ('double1','{ "number": "301", "category": "double", "capacity": 2, "features" : [ "tv", "ocean view" ] }');
INSERT INTO room (id,data)
    VALUES ('double2','{ "number": "302", "category": "double", "capacity": 2, "features" : [ "tv" ] }');
INSERT INTO room (id,data)
    VALUES ('quad1','{ "number": "202", "category": "quadruple", "capacity": 4, "features" : [ "tv", "ocean view", "terrace" ] }');
INSERT INTO room (id,data)
    VALUES ('apartment1','{ "number": "401", "category": "apartment", "capacity": 6, "features" : [ "living room", "bar", "workplace", "tv" ] }');

INSERT INTO reservation (id,data,rooms)
    VALUES (1,'{ "start": "2019-02-15", "end": "2019-02-17", "state": "processed", "price": 175 }', ARRAY[ 'single1', 'double1' ]);
INSERT INTO reservation (id,data,rooms)
    VALUES (2,'{ "start": "2020-11-15", "end": "2020-11-18", "state": "checked-in", "price": 200 }', ARRAY[ 'double1', 'double2' ]);
INSERT INTO reservation (id,data,rooms)
    VALUES (3,'{ "start": "2021-02-15", "end": "2021-02-23", "state": "confirmed", "price": 275 }', ARRAY[ 'single1', 'double1', 'quad1' ]);
INSERT INTO reservation (id,data,rooms)
    VALUES (4,'{ "start": "2020-11-15", "end": "2020-11-21", "state": "checked-in", "price": 250 }', ARRAY[ 'single1', 'quad1' ]);
INSERT INTO reservation (id,data,rooms)
    VALUES (5,'{ "start": "2021-10-15", "end": "2021-10-19", "state": "confirmed", "price": 300 }', ARRAY[ 'apartment1' ]);


--
-- INDEXES
CREATE INDEX room_index ON room ((data->>'number'));
CREATE INDEX customer_index ON customer ((data -> 'name' ->> 'last');


--   Vyjádřete alespoň 4 výrazy typu update
--     Nenahrazujte celý dokument, ale použijte k tomuto účelu vhodné operátory, např. ||, -, #-
--     Alespoň 1x použijte každou z následujících funkcí: jsonb_set, jsonb_insert
--     Alespoň 1x vykonejte update výraz nad vnořeným dokumentem
--     Alespoň 1x vykonejte update výraz nad polem
--
-- UPDATE
-- Room renovation

--

--

--



--   Vyjádřete alespoň 4 dotazy
--     Použijte klauzule WHERE, GROUP BY, HAVING a ORDER BY, a to každou z nich alespoň 1x
--     Alespoň 1x realizujte vnořený dotaz
--     Alespoň 2x reprezentujte výsledek celého dotazu v datovém typu jsonb, přičemž pokaždé k tomuto účelu použijte jinou funkci, např. to_json[b], array_to_json, row_to_json anebo json[b]_object
--
-- SELECT
--

--

-- Find future reservations by start.
SELECT
    c->data->'name'->>'last' AS lastName,
    r->data->>'start' arrivingOn
FROM reservation AS r
WHERE (
    (data->>'start')::DATE > CURRENT_DATE AND
    r->id IN (
        SELECT c->reservations FROM customer AS c
    )
)
ORDER BY (r->data->>'start')::DATE;
-- SUM the expected profit made.
SELECT
    SUM ((data ->> 'price')::INTEGER) AS expected_reservation_payed_sum,
FROM reservation;