DROP TABLE IF EXISTS actors;
DROP TABLE IF EXISTS movies;

CREATE TABLE actors (
	id TEXT PRIMARY KEY,
	data JSON,
	movies TEXT[]
);


CREATE TABLE movies (
	id TEXT PRIMARY KEY,
	data JSONB
);

INSERT INTO actors (id,data,movies) VALUES ('trojan','{ "name": { "first": "Ivan", "last": "Trojan" }, "year": 1964}', ARRAY[ 'samotari', 'medvidek', 'karamazovi' ]);
INSERT INTO actors (id,data,movies) VALUES ('machacek','{"name": { "first": "Jiri", "last": "Machacek" }, "year": 1966}', ARRAY[ 'medvidek', 'vratnelahve', 'samotari' ]);
INSERT INTO actors (id,data,movies) VALUES ('schneiderova','{"name": { "first": "Jitka", "last": "Schneiderova" }, "year": 1973}', ARRAY[ 'samotari' ]);
INSERT INTO actors (id,data,movies) VALUES ('sverak','{"name": { "first": "Zdenek", "last": "Sverak" }, "year": 1936}', ARRAY[ 'vratnelahve' ]);
INSERT INTO actors (id,data) VALUES ('geislerova','{"name": { "first": "Anna", "last": "Geislerova" }, "year": 1976}');
INSERT INTO actors (id,data,movies) VALUES ('vilhelmova','{"name": { "first": "Tatiana", "last": "Vilhelmova" }, "year": 1978}', ARRAY[ 'medvidek' ]);
INSERT INTO actors (id,data,movies) VALUES ('menzel','{   "name": { "last": "Menzel", "first": "Jiri" }, "year": 1938}', ARRAY[ 'medvidek' ]);

INSERT INTO movies (id,data)
	VALUES ('samotari','{ "title": { "cs": "Samotari", "en": "Loners" }, "year": 2000, "rating": 84, "length": 103, "actors": [ "trojan", "machacek", "schneiderova" ], "genres": [ "comedy", "drama" ], "country": [ "CZ", "SI" ] }');
INSERT INTO movies (id,data)
	VALUES ('medvidek','{ "title" : "Medvidek", "year": 2007, "rating": 53, "length": 100, "director": { "first": "Jan", "last": "Hrebejk" }, "actors": [ "trojan", "machacek", "vilhelmova", "issova", "menzel" ], "genres": [ "comedy", "drama" ], "country": [ "CZ" ] }');
INSERT INTO movies (id,data)
	VALUES ('vratnelahve','{ "title": { "cs": "Vratne lahve", "en": "Empties" }, "year": 2006, "rating":76, "length":99, "director": { "first": "Jan", "last": "Sverak" }, "actors": [ "sverak", "machacek", "schneiderova" ], "genres": "comedy", "country": "CZ" }');
INSERT INTO movies (id,data)
	VALUES ('zelary','{ "title": "Zelary", "year": 2003, "rating":81, "length":142, "director": { "last": "Trojan", "first": "Ondrej" }, "actors": [ ], "genres": [ "romance", "drama" ], "country": [ "CZ", "SK", "AT" ] }');
INSERT INTO movies (id,data)
	VALUES ('stesti','{ "title": "Stesti", "year": 2005, "rating": 72, "length": 100, "director": { "last": "Slama", "first": "Bohdan" }, "awards": [ { "type": "Czech Lion", "year": 2005 } ] }');
INSERT INTO movies (id,data)
	VALUES ('kolja','{ "title": "Kolja", "year": 1996, "rating":86, "length":105, "awards": [ { "type": "Czech Lion", "year": 1996 }, { "type": "Academy Awards", "category": "A", "year": 1996 } ] }');

-- Page 7 --

SELECT '{           "title": { "cs": "Samotari", "en": "Loners" }, "year": 2.0e+3, "rating": 84, "length": 103, "actors": [ "trojan", "machacek", "schneiderova" ], "genres": [ "comedy", "drama" ], "country": [ "CZ", "SI" ] }'::json;

SELECT '{ "title": { "cs": "Samotari", "en": "Loners" }, "year": 2.0e+3, "rating": 84, "length": 103, "actors": [ "trojan", "machacek", "schneiderova" ], "genres": [ "comedy", "drama" ], "country": [ "CZ", "SI" ] }'::jsonb;

-- Page 8 --

CREATE TABLE actors (
  id TEXT PRIMARY KEY,
  data JSON,
  movies TEXT[]
);

CREATE TABLE movies (
  id TEXT PRIMARY KEY,
  data JSONB
);

\dt[+]

\d[+] actors

\d[+] movies
-- Page 9 --

INSERT INTO movies (id,data) VALUES ('medvidek','{ "title" : "Medvidek", "year": 2007, "rating": 53, "length": 100 }');

INSERT INTO movies (id,data) VALUES ('zelary','{ "title": "Zelary", "year": 2003, "rating":81, "length":142, "actors": [ ], "genres": [ "romance", "drama" ] }');

INSERT INTO movies (id,data) VALUES ('kolja','{ "title": "Kolja", "year": 1996, "rating":86, "length":105, "awards": [ { "type": "Czech Lion", "year": 1996 }, { "type": "Academy Awards", "category": "A", "year": 1996 } ] }');

-- Page 10 --

UPDATE movies SET data = data || '{"id": "medvidek" }' WHERE id = 'medvidek';

UPDATE movies SET data = data - 'id' WHERE id = 'medvidek';

UPDATE movies SET data = data - ARRAY['actors','genres'] WHERE id = 'zelary';

UPDATE movies SET data = data #- '{awards,0}' WHERE id = 'kolja';

-- Page 11 --

DELETE FROM movies;

-- Page 12 --

SELECT data -> 'name' AS name FROM actors;

SELECT data ->> 'name' AS name FROM actors;

SELECT * FROM actors WHERE data -> 'name' ->> 'first' = 'Ivan';

SELECT data ->> 'name' ->> 'last' AS lastname FROM actors;

-- Page 13 --

SELECT data #> '{awards,0}' -> 'type' AS award FROM movies;

SELECT data #>> '{actors,1}' AS name FROM movies;

-- Page 14 --

SELECT data -> 'title' FROM movies WHERE data @> '{"length" : 100}';

SELECT data -> 'title' FROM movies WHERE '{"length" : 103}' <@ data;

SELECT data -> 'title' FROM movies WHERE data ? 'awards';

-- Page 15 --

SELECT data->'title' AS title FROM movies WHERE data ?| ARRAY['awards','actors'];

SELECT data->'title' AS title FROM movies WHERE data ?& ARRAY['awards','actors'];

-- Page 16 --

SELECT data -> 'length' FROM movies WHERE data ->> 'length' > '100';

SELECT data -> 'length' FROM movies WHERE (data ->> 'length')::INTEGER > 100;

-- Page 17 --

SELECT to_jsonb(movies) AS movies_json FROM actors;

SELECT array_to_json(movies, true) AS movies_json FROM actors;

SELECT row_to_json(row, true) FROM (SELECT * FROM actors) row;

-- Page 18 --

SELECT jsonb_object(ARRAY['id',id,'type', data->>'actors']) FROM movies;

SELECT jsonb_object(ARRAY['id','actors'], ARRAY[id, data->>'actors']) FROM movies;

-- Page 19 --

SELECT data FROM movies WHERE jsonb_array_length(data -> 'actors') > 3;

SELECT json_each(data) FROM actors;

SELECT jsonb_each_text(data) FROM movies;

SELECT jsonb_object_keys(data) FROM movies WHERE id = 'kolja';

-- Page 20 --

SELECT jsonb_array_elements(data -> 'actors') FROM movies WHERE id = 'medvidek';

SELECT id, jsonb_typeof(data -> 'title') FROM movies;

-- Page 21 --

UPDATE movies SET data=jsonb_set(data, '{actors,1}', '"geislerova"') WHERE data->>'title'='Medvidek';

UPDATE movies SET data=jsonb_insert(data, '{actors,1}', '"machacek"') WHERE data->>'title'='Medvidek';

SELECT jsonb_pretty(data) FROM movies;

-- Page 23 --

SELECT
  MIN ((data ->> 'length')::INTEGER) AS min_length,
  MAX ((data ->> 'length')::INTEGER) AS max_length,
  AVG ((data ->> 'length')::INTEGER) AS average_length,
  SUM ((data ->> 'length')::INTEGER) AS sum_length,
  COUNT (data ->> 'length') AS count_movies,
  EVERY ((data ->> 'length')::INTEGER > 100) AS all_long
FROM movies;

-- Page 24 --

CREATE INDEX movies_index ON movies ((data->>'year'));

CREATE INDEX actors_index ON actors ((data -> 'name' ->> 'last'), (data ->> 'year') DESC);

\d movies

\d actors

-- Page 25: Exercise 4 --
SELECT data->'name'->>'first' AS name, data->'name'->>'last' AS surname
FROM actors
WHERE (data->>'year')::INTEGER > 1966
ORDER BY (data->>'year')::INTEGER DESC;

-- Page 26: Exercise 5 --
SELECT data -> 'title' ->> 'cs' AS czech_title , data -> 'title' ->> 'en' AS english_title
FROM movies
WHERE data->'title' ?& ARRAY['cs','en'];

-- Page 27: Exercise 6 --
SELECT data -> 'title', data -> 'rating'
FROM movies
WHERE data->'genres' @> '["comedy", "drama"]' OR (data->>'rating')::INTEGER > 80;

-- Page 28: Exercise 7 --
SELECT data->>'title' AS title
FROM movies
WHERE data->'awards' @> '[{"type" : "Czech Lion"}]'::jsonb;

-- Page 29: Exercise 8 --
SELECT data->'title' AS title, data->'year' AS year, data->'director' AS director
FROM movies
WHERE data ? 'director' AND (data->'year')::INTEGER > 2000 AND (data->'year')::INTEGER < 2006
ORDER BY (data->'year')::INTEGER DESC;

-- Page 30: Exercise 9 --
SELECT data->>'title'
FROM movies
WHERE jsonb_typeof(data->'title')::TEXT = 'string' AND data->>'title' = 'Vratne lahve' OR jsonb_typeof(data->'title')::TEXT = 'object' AND data->'title'->>'cs' = 'Vratne lahve';

-- Page 31: Exercise 10 --
SELECT data->>'title', jsonb_array_length(data -> 'actors')
FROM movies
WHERE jsonb_array_length(data -> 'actors') > (
  SELECT AVG (jsonb_array_length(data -> 'actors'))
  FROM movies
);

-- Page 32: Exercise 11 --
SELECT jsonb_pretty(
  jsonb_object(
    ARRAY['title','year','actors'],
    ARRAY[data->>'title', data->>'year', jsonb_array_length(data -> 'actors')::TEXT]
    )
  )
FROM movies
WHERE (data ->> 'year')::INTEGER > 2000 AND data ? 'actors'
ORDER BY jsonb_array_length(data -> 'actors') DESC;