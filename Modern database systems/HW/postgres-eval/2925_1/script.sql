-- clear the database first
DROP TABLE IF EXISTS trams;
DROP TABLE IF EXISTS depots;

-- define schema
CREATE TABLE depots(
    id INT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    data JSON
);

CREATE TABLE trams(
    id INT PRIMARY KEY,
    depot_id INT DEFAULT NULL,
    data JSONB,
    CONSTRAINT fk_depot FOREIGN KEY(depot_id) REFERENCES depots(id) ON DELETE SET NULL
);

CREATE INDEX tram_driver_name_index ON trams ((data -> 'driver' ->> 'name'));


------------------
-- vkládání dat --
------------------

INSERT INTO depots(id, name, data) VALUES (
    1, 'Hostivař', '{
        "gps_coords": [12.0, 15.0]
    }'
);
INSERT INTO depots(id, name, data) VALUES (
    2, 'Florenc', '{
        "gps_coords": [13.0, 15.1]
    }'
);
INSERT INTO depots(id, name, data) VALUES (
    3, 'Háje', '{
        "gps_coords": [14.0, 15.2]
    }'
);
INSERT INTO depots(id, name, data) VALUES (
    4, 'Kobylisy', '{
        "gps_coords": [15.0, 15.3]
    }'
);
INSERT INTO depots(id, name, data) VALUES (
    5, 'Kačerov', '{
        "gps_coords": [16.0, 15.4]
    }'
);


INSERT INTO trams(id, depot_id, data) VALUES (
    1001, 1, '{
        "served_lines": [17, 16, 24],
        "driver": {
            "name": "John Doe"
        },
        "travelled_distance_km": 127843.56,
        "code": "AKSNCUJASDKL"
    }'
);
INSERT INTO trams(id, depot_id, data) VALUES (
    1002, 1, '{
        "served_lines": [26, 24, 23],
        "driver": {
            "name": "Peter Doe"
        },
        "travelled_distance_km": 67843.56,
        "code": "56SADULMWLDAS"
    }'
);
INSERT INTO trams(id, depot_id, data) VALUES (
    1003, 3, '{
        "served_lines": [18],
        "driver": {
            "name": "Peter Doe"
        },
        "travelled_distance_km": 57.5
    }'
);
INSERT INTO trams(id, depot_id, data) VALUES (
    1004, 5, '{
        "served_lines": [2, 10, 12, 24],
        "driver": {
            "name": "Bob Doe"
        },
        "travelled_distance_km": 5757.5,
        "code": "123456789"
    }'
);
INSERT INTO trams(id, depot_id, data) VALUES (
    1005, 5, '{
        "served_lines": [18, 19],
        "driver": {
            "name": "Bob Doe"
        },
        "travelled_distance_km": 1234.5
    }'
);


--------------------------------
-- demonstrace JSON operátorů --
--------------------------------

-- každý z operátorů
-- -> ->> #> #>>
-- a zřetězení -> a ->>
SELECT trams.id AS tram_id, trams.data #> '{served_lines}' AS all_served_lines FROM trams;
SELECT trams.id AS tram_id, CONCAT('linka ', trams.data #>> '{served_lines, 0}') AS some_served_line FROM trams;
SELECT trams.id AS tram_id, trams.data -> 'driver' ->> 'name' AS driver FROM trams;

-- filtrační JSON operátory
SELECT trams.id AS tram_id FROM trams WHERE trams.data ? 'code';
SELECT trams.id AS tram_id FROM trams WHERE (trams.data -> 'driver') @> '{"name": "John Doe"}';


-------------------------------
-- demonstrace update výrazů --
-------------------------------

-- změníme kód tramvaje
UPDATE trams SET data = data || '{"code": "ABCDEFGH"}' WHERE id = 1001;

-- změníme kód tramvaje pomocí jsonb_set
UPDATE trams SET data = jsonb_set(data, '{code}', '"HGFEDCBA"') WHERE id = 1001;

-- vnořený dokument: přidáme info o řidiči
UPDATE trams SET data = jsonb_insert(data, '{driver, favourite_color}', '"green"') WHERE id = 1001;

-- pole: odebereme linku 16
UPDATE trams SET data = jsonb_set(data, '{served_lines}',
    ( -- napočítáme nový seznam linek tak, že ten starý přefiltrujeme a zahodíme 16ku
        WITH tram_line AS (SELECT jsonb_array_elements(data -> 'served_lines') AS tram_line FROM trams WHERE id = 1001)
        SELECT
        (array_agg(tram_line)) FROM tram_line WHERE tram_line::INT <> 16
    )
) WHERE id = 1001;


----------------------------
-- demonstrace dotazování --
----------------------------

-- všechna depa jako seznam json objektů seřazená podle jména
SELECT row_to_json(row, true) AS depot FROM (SELECT * FROM depots) row ORDER BY row.name ASC;

-- přetypování: všechny tramvaje, které ujely alespoň 100km
SELECT trams.id AS tram_id FROM trams WHERE (trams.data -> 'travelled_distance_km')::REAL > 100;

-- kolik ujely tramvaje dohromady?
SELECT SUM((trams.data -> 'travelled_distance_km')::REAL) AS total_distance_km FROM trams;

-- kolik tramvají řídí každý řidič? (ale alespoň dvě)
SELECT trams.data -> 'driver' ->> 'name' AS driver, COUNT(*) AS count FROM trams GROUP BY driver HAVING COUNT(*) >= 2;

-- chceme řidiče každé tramvaje
SELECT row_to_json(row, true) AS tram_driver FROM (SELECT trams.id AS tram_id, trams.data -> 'driver' AS driver FROM trams) row;

-- vnořený dotaz: pro každou tramvaj najdeme depo
SELECT trams.id AS tram_id, (SELECT row_to_json(depots, false) FROM depots WHERE id = trams.depot_id) AS depot FROM trams;
