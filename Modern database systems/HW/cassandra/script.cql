/* bastianl - theme: hotel */

/* DROP TABLE m201_bastianl.customer; */
/* DROP TABLE m201_bastianl.room; */
/* DROP TABLE m201_bastianl.reservation; */

/* CREATE tables (3 - customer, room, reservation)*/
CREATE TABLE customer (
    id INT,
    name TEXT,
    address TUPLE<TEXT, TEXT, TEXT>,
    email TEXT,
    country TEXT,
    PRIMARY KEY (country, name)
);
/* 'Helper'/extra entity - to model the domain. */
CREATE TABLE room (
    id INT,
    r_category TEXT,
    r_number INT,
    capacity INT,
    features SET<TEXT>,
    PRIMARY KEY (id)
);
CREATE TABLE reservation (
    id INT,
    start DATE,
    end DATE,
    customersInRooms MAP<TEXT, INT>,
    notes LIST<TEXT>,
    PRIMARY KEY (id)
);


/* SECONDARY indexes. */
CREATE INDEX ON customer (id);
CREATE INDEX ON reservation (start);

/* INSERT Data */
/* INSERT Customer data */
INSERT INTO customer (id, name, address, email, country)
VALUES (
    1,
    'Jake Gyllenhaal',
    ('Hollywood Boulevard, Vine St', 'Los Angeles', 'CA 90028'),
    'gyllenhaalj@gmail.com',
    'USA'
);
INSERT INTO customer (id, name, address, email, country)
VALUES (
    2,
    'Magie Gyllenhaal',
    ('Hollywood Boulevard, Vine St', 'Los Angeles', 'CA 90028'),
    'gyllenhaalm@gmail.com',
    'USA'
);
INSERT INTO customer (id, name, address, email, country)
VALUES (
    3,
    'Nicolas Cage',
    ('5600 Sunset Blvd', 'Los Angeles', 'CA 90028'),
    'cage@goodactor.not',
    'USA'
);
INSERT INTO customer (id, name, address, email, country)
VALUES (
    4,
    'Henry Cavill',
    ('Castlehill', 'Edinburgh', 'EH1 2NG'),
    'cavill@thewitcher.pl',
    'GBR'
);
INSERT INTO customer (id, name, address, email, country)
VALUES (
    5,
    'Zdenek Sverak',
    ('Malostranske namesti', 'Praha', '11800'),
    'sverak@atlas.cz',
    'CZE'
);
/* INSERT Rooms */
INSERT INTO room (id, r_category, r_number, capacity, features)
VALUES (
    1,
    'single',
    201,
    1,
    { 'workplace', 'walk-in shower' }
);
INSERT INTO room (id, r_category, r_number, capacity, features)
VALUES (
    2,
    'double',
    301,
    2,
    { 'tv', 'ocean view' }
);
INSERT INTO room (id, r_category, r_number, capacity, features)
VALUES (
    3,
    'double',
    302,
    2,
    { 'tv', 'walk-in shower' }
);
INSERT INTO room (id, r_category, r_number, capacity, features)
VALUES (
    4,
    'quad',
    202,
    4,
    { 'tv', 'ocean view', 'terrace' }
);
INSERT INTO room (id, r_category, r_number, capacity, features)
VALUES (
    5,
    'appartment',
    401,
    6,
    { 'living room', 'workplace', 'tv' }
);
/* INSERT Reservations */
INSERT INTO reservation (id, start, end, customersInRooms,notes)
VALUES (
    1,
    '2019-02-15',
    '2019-02-17',
    { 'Jake Gyllenhaal' : 301, 'Magie Gyllenhaal' : 301 },
    ['late checkin']
);
INSERT INTO reservation (id, start, end, customersInRooms,notes)
VALUES (
    2,
    '2020-11-15',
    '2020-11-23',
    { 'Jake Gyllenhaal' : 302, 'Magie Gyllenhaal' : 302, 'Nicolas Cage' : 201 },
    ['late checkin', 'ordered special saloon']
);
INSERT INTO reservation (id, start, end, customersInRooms,notes)
VALUES (
    3,
    '2021-02-15',
    '2021-02-23',
    { 'Nicolas Cage' : 201, 'Henry Cavill' : 401 },
    ['late checkout', 'will order massages']
);
INSERT INTO reservation (id, start, end, customersInRooms,notes)
VALUES (
    4,
    '2020-11-15',
    '2020-11-21',
    { 'Nicolas Cage' : 301 },
    ['late checkout', 'will order massages']
);
INSERT INTO reservation (id, start, end, customersInRooms,notes)
VALUES (
    5,
    '2021-10-15',
    '2021-10-19',
    { 'Zdenek Sverak' :  202 },
    ['late checkin']
);


/* UPDATE */
/* SET - Room feature changes.*/
/* Upgrade in room 401. (add) */
UPDATE room
SET
    features = features + { 'minibar' }
where id = 5;
/* TV broke in room 302. (remove)*/
UPDATE room
SET
    features = features - { 'tv' }
where id = 4;
/* Renovation in room 201. (replace) */
UPDATE room
SET
    features = { 'bath', 'tv' }
where id = 1;
/* MAP - Room occupancy changes. */
/* One customer is not coming (remove)*/
UPDATE reservation
SET
    customersInRooms = customersInRooms - { 'Nicolas Cage' }
WHERE id = 2;
/* Swap rooms. (replace)*/
UPDATE reservation
SET
    customersInRooms = { 'Nicolas Cage' : 401, 'Henry Cavill' : 201 }
WHERE id = 3;
/* One more guest. (add) */
UPDATE reservation
SET
    customersInRooms = customersInRooms + { 'Jake Gyllenhaal' : 302  }
WHERE id = 5;
/* LIST - Notes being changed. */
/* One customer is not coming (remove)*/
UPDATE reservation
SET
    notes = notes - [ 'late checkin' ]
WHERE id = 2;
/* Swap customersInRooms + different customer instead of original. (replace) */
UPDATE reservation
SET
    notes = [ 'do not disturb' ]
WHERE id = 3;
/* One more guest. (add) */
UPDATE reservation
SET
    notes = notes + [ 'late checkin' ]
WHERE id = 5;



/* SELECT */
/* Find reservations starting past November 14 2020. */
SELECT *
FROM reservation
WHERE start > '2020-11-14'
ALLOW FILTERING;
/* Find visits of customer Nicolas Cage. */
SELECT *
FROM reservation
WHERE customersInRooms CONTAINS KEY 'Nicolas Cage'
ALLOW FILTERING;
/* Find customers from the USA and order them by name */
SELECT *
FROM customer
WHERE country = 'USA'
ORDER BY name;