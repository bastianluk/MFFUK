// -----------------------------------------------------------------------------

db.actors.save({
    _id: "trojan",
    name: { first: "Ivan", last: "Trojan" }, year: 1964,
    movies: [ "samotari", "medvidek", "karamazovi" ]
});
db.actors.save({
    _id: "machacek",
    name: { first: "Jiri", last: "Machacek" }, year: 1966,
    movies: [ "medvidek", "vratnelahve", "samotari" ]
});
db.actors.save({
    _id: "schneiderova",
    name: { first: "Jitka", last: "Schneiderova" }, year: 1973,
    movies: [ "samotari" ]
});
db.actors.save({
    _id: "sverak",
    name: { first: "Zdenek", last: "Sverak" }, year: 1936,
    movies: [ "vratnelahve" ]
});
db.actors.save({
    _id: "geislerova",
    name: { first: "Anna", last: "Geislerova" }, year: 1976
});
db.actors.save({
    _id: "vilhelmova",
    name: { first: "Tatiana", last: "Vilhelmova" }, year: 1978,
    movies: [ "medvidek"]
});
db.actors.save({
    _id: "menzel",
    name: { last: "Menzel", first: "Jiri" }, year: 1938,
    movies: "medvidek"
});

db.movies.save({
    _id: "samotari",
    title: { cs: "Samotari", en: "Loners" },
    year: 2000, rating: 84,
    actors: [ "trojan", "machacek", "schneiderova" ],
    genres: [ "comedy", "drama" ], country: [ "CZ", "SI" ], length: 103
});
db.movies.save({
    _id: "medvidek",
    title: "Medvidek",
    year: 2007, director: { first: "Jan", last: "Hrebejk" }, rating: 53,
    actors: [ "trojan", "machacek", "vilhelmova", "issova", "menzel" ],
    genres: [ "comedy", "drama" ], country: [ "CZ" ], length: 100
});
db.movies.save({
    _id: "vratnelahve",
    title: { cs: "Vratne lahve", en: "Empties" },
    year: 2006, director: { first: "Jan", last: "Sverak" }, rating: 76,
    actors: [ "sverak", "machacek", "schneiderova" ],
    genres: "comedy", country: "CZ", length: 99
});
db.movies.save({
    _id: "zelary",
    title: "Zelary",
    year: 2003, director: { last: "Trojan", first: "Ondrej" }, rating: 81,
    actors: [ ],
    genres: [ "romance", "drama" ], country: [ "CZ", "SK", "AT" ], length: 142
});
db.movies.save({
    _id: "stesti",
    title: "Stesti",
    year: 2005, director: { last: "Slama", first: "Bohdan" }, rating: 72,
    length: 100,
    awards: [
        { type: "Czech Lion", year: 2005 }
    ]
});
db.movies.save({
    _id: "kolja",
    title: "Kolja",
    year: 1996, rating: 86,
    length: 105,
    awards: [
        { type: "Czech Lion", year: 1996 },
        { type: "Noname Awards", category: "A", year: 2005 }
    ]
});

// -----------------------------------------------------------------------------

%%%%% Page 7 %%%%%

use contos
db = db.getSiblingDB('contos')

show databases
show dbs
db.adminCommand('listDatabases')

%%%%% Page 8 %%%%%

db.createCollection("actors")

show collections
db.getCollectionNames()

%%%%% Page 10: Exercise 1 %%%%%

db.actors.insert({ _id: "trojan", name: "Ivan Trojan" });
db.actors.insert({ _id: 2, name: "Jiri Machacek" });
db.actors.insert({ _id: ObjectId(), name: "Jitka Schneiderova" });
db.actors.insert({ name: "Zdenek Sverak" });

db.actors.find();

%%%%% Page 12: Exercise 2 %%%%%

db.actors.update({ _id: "trojan" }, { name: "Ivan Trojan", year: 1964 });
db.actors.update({ name: "Ivan Trojan", year: { $lt: 2000 } }, { name: "Ivan Trojan", year: 1964 });

db.actors.find({ _id: "trojan" });

%%%%% Page 13: Exercise 3 %%%%%

db.actors.update({ _id: "geislerova" }, { name: "Anna Geislerova" }, { upsert: true });

%%%%% Page 14: Exercise 4 %%%%%

db.actors.update({ _id: "trojan" }, { _id: 1, name: "Ivan Trojan", year: 1964 });

%%%%% Page 15: Exercise 5 %%%%%

db.actors.update(
    { _id: "trojan" },
    {
        $set: { year: 1964, age: 52 },
        $inc: { rating: 1 },
        $push: { movies: { $each: [ "samotari", "medvidek" ] } }
    }
)

db.actors.update(
    { year: { $lt: 2000 } },
    { $set: { rating: 3 } },
    { multi: true }
)

%%%%% Page 17: Exercise 6 %%%%%

db.actors.save({ name: "Tatiana Vilhelmova" });
db.actors.save({ _id: 6, name: "Sasa Rasilov" });

db.actors.save({ _id: "trojan", name: "Ivan Trojan", year: 1964 });

%%%%% Page 19: Exercise 7 %%%%%

db.actors.remove({ _id: "geislerova" });

db.actors.remove({ year: { $lt: 2000 } }, { justOne: true });

db.actors.remove({ });

%%%%% Page 24: Exercise 8 %%%%%

db.actors.find()
db.actors.find( { } )

db.actors.find( { _id: "trojan" } )
db.actors.find( { "name.first": "Ivan", year: 1964 } )
db.actors.find( { year: { $gte: 1960, $lte: 1980 } } )

db.actors.find( { movies: { $exists: true } } )
db.actors.find( { movies: "medvidek" } )
db.actors.find( { movies: { $in: ["medvidek", "vratnelahve" ] } } )
db.actors.find( { movies: { $all: [ "medvidek", "samotari" ] } } )

%%%%% Page 25: Exercise 8 %%%%%

db.actors.find({ $or: [ { year: 1964 }, { rating: { $gte: 3 } } ] });
db.actors.find({ rating: { $not: { $gte: 3 } } });

db.actors.find({ }, { name: 1, year: 1});
db.actors.find({ }, { movies: 0, _id: 0 });
db.actors.find({ }, { name: 1, movies: { $slice: 2 }, _id: 0 });

db.actors.find().sort({ year: 1, name: -1 });
db.actors.find().sort({ name: 1 }).skip(1).limit(2);
db.actors.find().sort({ name: 1 }).limit(2).skip(1);

%%%%% Page 26: Exercise 9 %%%%%

db.actors.find(
    { year: 1966, "name.first": "Jiri" }
);

db.actors.find(
    { "name.first": "Jiri", year: 1966, }
);

db.actors.find(
    { year: { $eq: 1966 }, "name.first": { $eq: "Jiri" } }
);

db.actors.find(
    { year: { $eq: 1966 }, "name.first": "Jiri" }
);

%%%%% Page 27: Exercise 10 %%%%%

/*INCORRECT*/
db.movies.find(
    { director: { first: "Jan", last: "Hrebejk" } }
);

/*INCORRECT*/
db.movies.find(
    { director: { last: "Hrebejk", first: "Jan" } }
);

db.movies.find(
    { "director.first": "Jan", "director.last": "Hrebejk" }
);

%%%%% Page 28: Exercise 11 %%%%%

db.actors.find(
    { "name.first": "Jiri", movies: "medvidek" },
    { name: 1, _id: 0 }
);

db.actors.find(
    { "name.first": "Jiri", movies: { $in : [ "medvidek" ] } },
    { name: 1, _id: 0 }
);

%%%%% Page 29: Exercise 12 %%%%%

db.movies.find(
    {
        year: { $gte: 2000, $lte: 2005 },
        director: { $exists: 1 }
    },
    { _id: 1 }
).sort(
    { rating: -1, year: 1 }
);

/*INCORRECT*/
db.movies.find(
    {
        year: { $gte: 2000 },
        year: { $lte: 2005 },
        director: { $exists: 1 }
    },
    { _id: 1 }
).sort(
    { rating: -1, year: 1 }
);

%%%%% Page 30: Exercise 13 %%%%%

db.actors.find(
    { movies: { $in: [ "medvidek", "samotari" ] } },
    { _id: 1 }
);

db.actors.find(
    {
        $or: [
            { movies: "medvidek" },
            { movies: "samotari" }
        ]
    },
    { _id: 1 }
);

%%%%% Page 31: Exercise 14 %%%%%

db.actors.find(
    { movies: { $all: [ "medvidek", "samotari" ] } },
    { _id: 1 }
);

db.actors.find(
    { $and: [ { movies: "medvidek" }, { movies: "samotari" } ] },
    { _id: 1 }
);

db.actors.find(
    { $and: [
        { movies: { $eq: "medvidek" } },
        { movies: { $eq: "samotari" } }
    ] },
    { _id: 1 }
);

db.actors.find(
    { $and: [
        { movies: { $in: [ "medvidek" ] } },
        { movies: { $in: [ "samotari" ] } }
    ] },
    { _id: 1 }
);

/*INCORRECT*/
db.actors.find(
    { movies: "medvidek", movies: "samotari" },
    { _id: 1 }
);
%%%%% Page 32: Exercise 15 %%%%%

db.movies.find(
    { $or: [
        { title: "Vratne lahve" },
        { "title.cs": "Vratne lahve" }
    ] },
    { title: 1, _id: 0 }
);

db.movies.find(
    { $or: [
        { title: { $eq: "Vratne lahve" } },
        { "title.cs": { $eq: "Vratne lahve" } }
    ] },
    { title: 1, _id: 0 }
);

%%%%% Page 33: Exercise 16 %%%%%

/*INCORRECT*/
db.movies.find(
    { awards: { type: "Czech Lion", year: 2005 } },
    { _id: 1, awards: 1 }
);

/*INCORRECT*/
db.movies.find(
    { awards: { $in : [ { type: "Czech Lion", year: 2005 } ]} },
    { _id: 1, awards: 1 }
);

/*INCORRECT*/
db.movies.find(
    { "awards.type": "Czech Lion", "awards.year": 2005 },
    { _id: 1, awards: 1 }
);

/*INCORRECT*/
db.movies.find(
    { $and: [
        { "awards.type": "Czech Lion" },
        { "awards.year": 2005 }
    ] },
    { _id: 1, awards: 1 }
);

db.movies.find(
    {
        awards: { $elemMatch: { type: "Czech Lion", year: 2005 } }
    },
    { _id: 1, awards: 1 }
);

%%%%% Page 34: Exercise 17 %%%%%

db.movies.find(
    {
        $or: [
            { genres: { $all: [ "comedy", "drama" ] } },
            { rating: { $gte: 80 } }
        ]
    },
    { _id: 1, country: { $slice: 2 } }
);

db.movies.find(
    {
        $or: [
            { $and: [ { genres: "comedy" }, { genres: "drama" } ] },
            { rating: { $gte: 80 } }
        ]
    },
    { _id: 1, country: { $slice: 2 } }
);


%%%%% Page 39: Exercise 18 %%%%%

db.actors.find({ movies: "medvidek" });

db.actors.find({ movies: "medvidek" }).explain();

db.actors.createIndex({ movies : 1 });

%%%%% Page 42: Exercise 19 %%%%%

db.movies.mapReduce(
  function() {
    emit(this.year, 1);
  },
  function(key, value) {
    return Array.sum(values);
  },
  {
    query: { year: { $gte: 2005 } },
    sort: { year: 1 },
    out: "statistics"
  }
)

db.statistics.find()

%%%%% Page 43: Exercise 20 %%%%%

db.actors.mapReduce(
    function() {
        emit(this.year, this.name);
    },
    function(key, values) {
        return values.sort();
    },
    {
        query: { year: { $lte: 2000 } },
        out: { inline: 1 }
    }
);

db.actors.mapReduce(
    function() {
        if (this.movies) {
            this.movies.forEach(
                function(m) { emit(m, 1); }
            );
        }
    },
    function(key, values) {
        return Array.sum(values);
    },
    { out: { inline: 1 } }
);

db.actors.mapReduce(
    function() {
        this.movies.forEach(
            function(m) { emit(m, 1); }
        );
    },
    function(key, values) {
        return Array.sum(values);
    },
    {
        query: { movies: { $exists: true } },
        out: { inline: 1 }
    }
);

db.actors.mapReduce(
    function() {
        if (Array.isArray(this.movies)) {
            this.movies.forEach(
                function(m) { emit(m, 1); }
            );
        } else {
            emit(this.movies, 1);
        }
    },
    function(key, values) {
        return Array.sum(values);
    },
    {
        query: { movies: { $exists: true } },
        out: { inline: 1 }
    }
);

%%%%% Drop Database %%%%%

db.dropDatabase();