// bastianl - theme - hotel

// Vykonání:
//  Vykonejte následující shell přikaz: cat $ScriptFile | neo4j-shell --path $DatabasePath --non-interactive
//  $DatabasePath je cestou k adresáři, který obsahuje Neo4j embedded databá

// reservation.start and reservation.end should be of type  or time but the version of neo4j on teh nosql server is too low and doesnt support it yet. 3.4 or higher is required.
CREATE
  (c1:CUSTOMER { id: "gylj", name: "Jake Gyllenhaal", country: "USA", email: "gyllenhaalj@gmail.com", year: 1980 }),
  (c2:CUSTOMER { id: "gylm", name: "Maggie Gyllenhaal", country: "USA", email: "gyllenhaalm@gmail.com", year: 1977 }),
  (c3:CUSTOMER { id: "cage", name: "Nicolas Cage", country: "USA", email: "cage@goodactor.not", year: 1964 }),
  (c4:CUSTOMER { id: "witcher", name: "Henry Cavill", country: "GBR", email: "cavill@thewitcher.pl", year: 1983 }),
  (c5:CUSTOMER { id: "sverak", name: "Zdenek Sverak", country: "CZE", email: "sverak@atlas.cz", year: 1936 }),
  (r1:ROOM { id: "single", number: "201", category: "single", capacity: 1 }),
  (r2:ROOM { id: "double1", number: "301", category: "double", capacity: 2 }),
  (r3:ROOM { id: "double2", number: "302", category: "double", capacity: 2 }),
  (r4:ROOM { id: "quad", number: "202", category: "quadruple", capacity: 4 }),
  (r5:ROOM { id: "apartment", number: "401", category: "apartment", capacity: 6 }),
  (r6:ROOM { id: "cellar", number: "666", category: "cellar", capacity: 0 }),
  (res1:RESERVATION { id: "res1", start: "2019-02-15", end: "2019-02-17", state: "processed" }),
  (res2:RESERVATION { id: "res2", start: "2020-11-15", end: "2020-11-18", state: "checked-in" }),
  (res3:RESERVATION { id: "res3", start: "2021-02-15", end: "2021-02-23", state: "confirmed" }),
  (res4:RESERVATION { id: "res4", start: "2020-11-15", end: "2020-11-21", state: "checked-in" }),
  (res5:RESERVATION { id: "res5", start: "2021-10-15", end: "2021-10-19", state: "confirmed" }),
  (res1)-[a1:ASSIGNED { occupancy: 1 }]->(r1),
  (res1)-[a2:ASSIGNED { occupancy: 2 }]->(r2),
  (res2)-[a3:ASSIGNED { occupancy: 2 }]->(r2),
  (res2)-[a4:ASSIGNED { occupancy: 2 }]->(r3),
  (res3)-[a5:ASSIGNED { occupancy: 1 }]->(r1),
  (res3)-[a6:ASSIGNED { occupancy: 2 }]->(r2),
  (res3)-[a7:ASSIGNED { occupancy: 3, children: 1 }]->(r4),
  (res4)-[a8:ASSIGNED { occupancy: 1 }]->(r1),
  (res4)-[a9:ASSIGNED { occupancy: 3 }]->(r4),
  (res5)-[a10:ASSIGNED { occupancy: 4 }]->(r5),
  (res1)-[o1:ORDEREDBY { price: 175 }]->(c1),
  (res2)-[o2:ORDEREDBY { price: 200 }]->(c2),
  (res3)-[o3:ORDEREDBY { price: 275 }]->(c3),
  (res4)-[o4:ORDEREDBY { price: 250 }]->(c5),
  (res5)-[o5:ORDEREDBY { price: 300 }]->(c4);

// Find customers from the USA or those who reserved an apartment - "find big spenders";
MATCH (c:CUSTOMER)
  WHERE c.country = "USA" or (c)<-[:ORDEREDBY]-(:RESERVATION)-[:ASSIGNED]->(:ROOM {category: "apartment"})
RETURN c.name;

// Get the average utilisation of rooms in the hotel (occupancy/capacity).
MATCH (r:ROOM)<-[a:ASSIGNED]-(:RESERVATION)
WITH avg(a.occupancy/r.capacity) AS averageUtilisation
RETURN averageUtilisation;

// Get confirmed (=> upcoming) reservations and order them by start date (even string ordering should be enough given by the format).
MATCH (res:RESERVATION { state: "confirmed" })
RETURN res
  ORDER BY res.start;

// Average reservation price
MATCH (res:RESERVATION)-[o:ORDEREDBY]->(:CUSTOMER)
WITH AVG(o.price) AS average
RETURN average;

// Find rooms that were used at some point and count the number of reservations connected, ordered by the count.
MATCH (r:ROOM)
OPTIONAL MATCH (r)<-[:ASSIGNED]-(res:RESERVATION)
WITH r, COUNT(res) AS reservationInRoomCount
RETURN r.number, reservationInRoomCount
  ORDER BY reservationInRoomCount DESC;


// CLEANUP
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n,r;
