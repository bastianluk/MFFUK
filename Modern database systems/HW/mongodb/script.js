/*

Zadání:
- Vytvořte JavaScript skript, který obsahuje sekvenci příkazů pracujících s MongoDB databází
- Explicitně vytvořte 2 kolekce pro entity různých typů
  - Tzn. použijte metodu createCollection
- Do každé kolekce vložte alespoň 5 dokumentů
  - Tyto dokumenty musí být realistické, netriviální, v rámci vybraného tématu a musí obsahovat vnořené objekty a pole
  - Propojte dokumenty pomocí referencí
  - Použijte operace insert i save, každou z nich alespoň 1x
- Vyjádřete 3 operace typu update (nepoužívejte pro tento účel operaci save)
  - Jednou bez update operátorů
  - Jednou s alespoň dvěma různými update operátory
  - Jednou za použití upsert módu
- Vyjádřete 5 operací typu find (obsahující netriviální selekci)
  - Použijte alespoň jeden logický operátor ($and, $or, $not)
  - Použijte operátor $elemMatch alespoň jednou na prvcích pole
  - Použijte alespoň jednou jak pozitivní, tak i negativní projekci
  - Použijte modifikátor sort
  - Popište významy všech dotazů v komentářích s použitím přirozeného jazyka
- Vyjádřete 1 MapReduce dotaz (netriviální, tj. takový, který nejde snadno vyjádřit použitím obyčejné find operace)
  - Popište význam této úlohy, obsahy dočasných párů klíč / hodnota a výsledný výstup
  - Nezapomeňte, že reduce funkce musí být asociativní, komutativní a idempotentní

Požadavky:
- V případě problémů se spuštěním mongo shell zavolejte export LC_ALL=C
- Používejte pouze Vaši databázi
  - Jméno takové databáze je identické s Vaším loginem, např. m201_student
- Nepřepínejte mezi databázemi uvnitř Vašeho skriptu
  - Tzn. nevolejte příkaz USE ani příkaz db.getSiblingDB('database')
  - Určete databázi použitím parametru volání mongo shellu (viz níže)
- Vytiskněte výstup všech dotazů (operací find)
  - Použijte db.collection.find().forEach(printjson);
- Vytiskněte výstup Vaší MapReduce úlohy pomocí out: { inline: 1 } option
  - Tzn. nepřesměrovávejte výstup do tzv. standalone kolekce

 */

// bastianl - theme - hotel


// CREATE - using 3 entities to model the domain better.
db.customer.save(
    {
        _id: "gylj",
        name: {
            first: "Jake",
            last: "Gyllenhaal"
        },
        birth_year: 1980,
        address: {
            street: "Hollywood Boulevard, Vine St",
            city: "Los Angeles",
            postalCode: "CA 90028",
            country: "USA"
        },
        email: "gyllenhaalj@gmail.com"
    }
);
db.customer.save(
    {
        _id: "gylm",
        name: {
            first: "Maggie",
            last: "Gyllenhaal"
        },
        birth_year: 1977,
        address: {
            street: "Hollywood Boulevard, Vine St",
            city: "Los Angeles",
            postalCode: "CA 90028",
            country: "USA"
        },
        email: "gyllenhaalm@gmail.com"
    }
);
db.customer.insert(
    {
        _id: "cage",
        name: {
            first: "Nicolas",
            last: "Cage"
        },
        birth_year: 1964,
        address: {
            street: "5600 Sunset Blvd",
            city: "Los Angeles",
            postalCode: "CA 90028",
            country: "USA"
        },
        email: "cage@goodactor.not"
    }
);
db.customer.save(
    {
        _id: "witcher",
        name: {
            first: "Henry",
            last: "Cavill"
        },
        birth_year: 1983,
        address: {
            street: "Castlehill",
            city: "Edinburgh",
            postalCode: "EH1 2NG",
            country: "GBR"
        },
        email: "cavill@thewitcher.pl"
    }
);
db.customer.save(
    {
        _id: "sverak",
        name: {
            first: "Zdenek",
            last: "Sverak"
        },
        birth_year: 1936,
        address: {
            street: "Malostranske namesti",
            city: "Praha",
            postalCode: "11800",
            country: "CZE"
        },
        email: "sverak@atlas.cz"
    }
);

db.room.save(
    {
        _id: "single",
        number: "201",
        category: "single",
        capacity: {
            normal: "1",
            extra: "0"
        },
        features: [
            "workplace",
            "walk-in shower"
        ]
    }
);
db.room.save(
    {
        _id: "double1",
        number: "301",
        category: "double",
        capacity: {
            normal: "2",
            extra: "1"
        },
        features: [
            "tv",
            "ocean view"
        ]
    }
);
db.room.save(
    {
        _id: "double2",
        number: "302",
        category: "double",
        capacity: {
            normal: "2",
            extra: "0"
        },
        features: [
            "tv",
            "walk-in shower"
        ]
    }
);
db.room.save(
    {
        _id: "quad",
        number: "202",
        category: "quad",
        capacity: {
            normal: "4",
            extra: "0"
        },
        features: [
            "tv",
            "ocean view",
            "terrace"
        ]
    }
);
db.room.save(
    {
        _id: "apartment",
        number: "401",
        capacity: {
            normal: "5",
            extra: "2"
        },
        features: [
            "living room",
            "workplace",
            "tv"
        ]
    }
);

db.reservation.save(
    {
        _id: "res1",
        period: {
            start: "2019-02-15",
            end: "2019-02-17"
        },
        state: "processed",
        customer_id: "gylj",
        assigned_room_id: "double1",
        price: 225
    }
);
db.reservation.save(
    {
        _id: "res2",
        period: {
            start: "2020-11-15",
            end: "2020-11-18"
        },
        state: "checked-in",
        customer_id: "gylm",
        assigned_room_id: "single",
        price: 135
    }
);
db.reservation.save(
    {
        _id: "res3",
        period: {
            start: "2021-02-15",
            end: "2021-02-23"
        },
        state: "confirmed",
        customer_id: "cage",
        assigned_room_id: "quad",
        price: 666
    }
);
db.reservation.save(
    {
        _id: "res4",
        period: {
            start: "2020-11-15",
            end: "2020-11-21"
        },
        state: "checked-in",
        customer_id: "sverak",
        assigned_room_id: "double2",
        price: 550
    }
);
db.reservation.save(
    {
        _id: "res5",
        period: {
            start: "2021-10-15",
            end: "2021-10-19"
        },
        state: "confirmed",
        customer_id: "witcher",
        assigned_room_id: "apartment",
        price: 750
    }
);


// UPDATE (3)
//com
//com
//com


// FIND (5)
//com
//com
//com
//com
//com


// MAPREDUCE (1)
//com


// CLEANUP
db.dropDatabase();