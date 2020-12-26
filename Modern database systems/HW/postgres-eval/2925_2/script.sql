CREATE TABLE companies (
  id INT PRIMARY KEY,
  name TEXT,
  headquarters JSON
);

CREATE TABLE games (
  id INT PRIMARY KEY,
  author INT REFERENCES companies (id),
  data JSONB
);


INSERT INTO companies (id, name, headquarters)
VALUES (
  1,
  'Broderbund',
  '{"city":"Eugene", "province":"Oregon", "country":"U.S."}'
);

INSERT INTO companies (id, name, headquarters)
VALUES (
  2,
  'Sierra On-Line',
  '{"city":"Los Angeles", "province":"California", "country":"U.S."}'
);

INSERT INTO companies (id, name, headquarters)
VALUES (
  3,
  'LucasArts',
  '{"city":"Letterman Digital Arts Center", "province":"San Francisco", "country":"U.S."}'
);

INSERT INTO companies (id, name, headquarters)
VALUES (
  4,
  'Blizzard Entertainment',
  '{"city":"Irvine", "province":"California", "country":"U.S."}'
);

INSERT INTO companies (id, name, headquarters)
VALUES (
  5,
  'Bohemia Interactive Studio',
  '{"city":"Prague", "country":"Czech Republic"}'
);


INSERT INTO games (id, author, data)
VALUES (
  1,
  1,
  '{"name":"Prince of Persia",
  "year":1989,
  "platforms":{"MS-DOS":true, "Apple II":true, "NES":true, "SNES":true},
  "versions":["1.00", "1.01", "1.1", "1.2"]
  }'
);

INSERT INTO games (id, author, data)
VALUES (
  2,
  2,
  '{"name":"Quest for Glory I: So You Want To Be A Hero",
  "year":1989,
  "platforms":{"MS-DOS":true, "Windows":true},
  "versions":["EGA", "VGA"]
  }'
);

INSERT INTO games (id, author, data)
VALUES (
  3,
  3,
  '{"name":"Monkey Island 1",
  "year":1990,
  "platforms":{"MS-DOS":true, "Amiga":true, "Apple II":true},
  "versions":["1.0", "1.1", "2.0"]
  }'
);

INSERT INTO games (id, author, data)
VALUES (
  4,
  4,
  '{"name":"Warcraft: Orcs & Humans",
  "year":1995,
  "platforms":{"MS-DOS":true, "Mac OS Classic":true},
  "versions":["1.0", "1.1", "1.22"]
  }'
);

INSERT INTO games (id, author, data)
VALUES (
  5,
  5,
  '{"name":"Arma 3",
  "year":2013,
  "platforms":{"Windows":true}
  }'
);


UPDATE games SET data = data || '{"platforms":{"MS-DOS":true}}' WHERE id = 2;
UPDATE games SET data = jsonb_set(data, '{platforms,Atari}', 'true') WHERE id = 1;
UPDATE games SET data = jsonb_insert(data, '{versions}', '["1.0", "1.5", "2.0"]') WHERE id = 5;
UPDATE games SET data = data #- '{versions,-1}' WHERE id = 4;

SELECT id, data FROM games WHERE (data ->> 'year')::INTEGER > 1990 ORDER BY (data ->> 'year')::INTEGER;
SELECT headquarters ->> 'country' AS country, COUNT(*) FROM companies GROUP BY headquarters ->> 'country' HAVING COUNT(*) > 2;
SELECT to_jsonb(data #>> '{name}') AS name FROM games WHERE data #> '{platforms}' ?| ARRAY['Windows', 'MS-DOS'];
SELECT row_to_json(row) AS data FROM (SELECT name, headquarters ->> 'country' AS country FROM companies) row;

CREATE INDEX games_index ON games ((data ->> 'name'));
