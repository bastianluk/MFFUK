-- bastian - theme - hotel


--
-- PRE exec CLEANUP
DROP TABLE IF EXISTS reservation;
DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS room;


--
-- CREATE TABLES
CREATE TABLE customer (
    id TEXT PRIMARY KEY,
    data JSONB
);
CREATE TABLE room (
    id TEXT PRIMARY KEY,
    data JSONB
);
CREATE TABLE reservation (
    id SERIAL PRIMARY KEY,
    data JSON,
    rooms TEXT[],
    customer_reference TEXT references customer(id)
);


--
-- INSERT
INSERT INTO customer (id,data)
    VALUES ('gyllenhaalj','{ "name": { "first" : "Jake", "last" : "Gyllenhaal" }, "country": "USA", "email": "gyllenhaalj@gmail.com", "year": 1980 }');
INSERT INTO customer (id,data)
    VALUES ('gyllenhaalm','{ "name": { "first" : "Magie", "last" : "Gyllenhaal" }, "country": "USA", "email": "gyllenhaalm@gmail.com", "year": 1977 }');
INSERT INTO customer (id,data)
    VALUES ('cage','{ "name": { "first" : "Nicolas", "middle" : "noonecares", "last" : "Cage" }, "country": "USA", "email": "cage@goodactor.not", "year": 1964 }');
INSERT INTO customer (id,data)
    VALUES ('cavill','{ "name": { "first" : "Henry", "last" : "Cavill" }, "country": "GBR", "email": "cavill@thewitcher.pl", "year": 1983 }');
INSERT INTO customer (id,data)
    VALUES ('sverak','{ "name": { "first" : "Zdenek", "last" : "Sverak" }, "country": "CZE", "email": "sverak@atlas.cz", "year": 1936 }');

INSERT INTO room (id,data)
    VALUES ('single1','{ "number": "201", "category": "single", "capacity": 1, "features" : [ "workplace" ] }');
INSERT INTO room (id,data)
    VALUES ('double1','{ "number": "301", "category": "double", "capacity": 2, "features" : [ "tv", "ocean view" ] }');
INSERT INTO room (id,data)
    VALUES ('double2','{ "number": "302", "category": "double", "capacity": 2, "features" : [ "tv" ] }');
INSERT INTO room (id,data)
    VALUES ('quad1','{ "number": "202", "category": "quadruple", "capacity": 4, "features" : [ "tv", "ocean view" ] }');
INSERT INTO room (id,data)
    VALUES ('apartment1','{ "number": "401", "category": "apartment", "capacity": 6, "features" : [ "living room", "bar", "workplace", "tv" ] }');

INSERT INTO reservation (id,data,rooms,customer_reference)
    VALUES (1,'{ "start": "2019-02-15", "end": "2019-02-17", "state": "processed", "price": 175 }', ARRAY[ 'single1', 'double1' ], 'gyllenhaalj');
INSERT INTO reservation (id,data,rooms,customer_reference)
    VALUES (2,'{ "start": "2020-11-15", "end": "2020-11-18", "state": "checked-in", "price": 200 }', ARRAY[ 'double1', 'double2' ], 'gyllenhaalm');
INSERT INTO reservation (id,data,rooms,customer_reference)
    VALUES (3,'{ "start": "2021-02-15", "end": "2021-02-23", "state": "confirmed", "price": 275 }', ARRAY[ 'single1', 'double1', 'quad1' ], 'cage');
INSERT INTO reservation (id,data,rooms,customer_reference)
    VALUES (4,'{ "start": "2020-11-15", "end": "2020-11-21", "state": "checked-in", "price": 250 }', ARRAY[ 'single1', 'quad1' ], 'sverak');
INSERT INTO reservation (id,data,rooms,customer_reference)
    VALUES (5,'{ "start": "2021-10-15", "end": "2021-10-19", "state": "confirmed", "price": 300 }', ARRAY[ 'apartment1' ], 'cavill');


--
-- INDEXES
CREATE INDEX room_index ON room ((data->>'number'));
CREATE INDEX customer_name_index ON customer ((data->'name'->>'last'));


-- UPDATE
-- An upgrade to a room.
UPDATE room SET data = jsonb_insert(data, '{features, 0}', '"minibar"') WHERE id = 'double2';
-- There was a typo in a name of one customer.
UPDATE customer SET data = jsonb_set(data, '{name,first}', '"Maggie"') WHERE id = 'gyllenhaalm';
-- One guest is not coming (and his room is not needed).
UPDATE reservation SET rooms = ARRAY_REMOVE(rooms, 'single1') WHERE id = 3;
-- TV broke in the quad room.
UPDATE room SET data = data #- '{features,0}' WHERE id = 'quad1';


--
-- SELECT
-- Reservation state report - count the rooms, that need to be cleaned - count the reservations grouped by state, but only processed (guest already left) or checked-in (guest maybe needs the room cleaned).
SELECT
    jsonb_object(ARRAY[
        'state', (data#>>'{state}'),
        'reservation_count', (COUNT (data))::TEXT
    ]) AS per_state_reservation_count
FROM reservation
GROUP BY (data#>>'{state}')
HAVING (data#>>'{state}') IN ('checked-in', 'processed');
-- Find all non-senior people, count them by county.
SELECT
    to_jsonb(ARRAY[
        (data->>'country'),
        (COUNT (*))::TEXT
    ]) AS per_country_nonsenior_count
FROM customer
WHERE (data->>'year')::INTEGER > 1950
GROUP BY (data->>'country');
-- Find future reservations by start and the rooms related to them.
SELECT
    jsonb_object(ARRAY[
        'id', ((data->>'start')::TEXT),
        'state', (data->>'start'),
        'rooms', array_to_string(rooms, ',')
    ]) AS future_reservations
FROM reservation
WHERE (data->>'start')::DATE > CURRENT_DATE
ORDER BY (data->>'start')::DATE;
-- SUM the expected profit made.
SELECT
    SUM ((data->>'price')::INTEGER) AS expected_reservation_payed_sum
FROM reservation;
-- Customer search - find the customer that have a middle name and list thier name, birth year and email (base info and contact).
SELECT
    (data->'name') AS name,
    (data->>'email') AS email,
    (data->'year') AS year
FROM customer
WHERE (data->'name') ? 'middle';
-- Find the rooms associated with the reservations ordered by the customer with the last name Cavill.
SELECT array_to_json(rooms) AS rooms
FROM reservation
WHERE (
    customer_reference IN (
        SELECT id
        FROM customer
        WHERE (data->'name') @> '{ "last" : "Cavill" }'
    )
);
-- Customer report - name and contact (email)
SELECT
    data #> '{name}' ->> 'last' AS lastName,
    data ->> 'email' AS email
FROM customer;
