#!/bin/bash

# bastian - theme: hotel

# Vytvořte script (bash script), který pracuje s databází Riak přes rozhraní HTTP pomocí nátroje cURL
# - Pro každý ze 3 entitních typů vložte alespoň 5 klíč / hodnota objektů
#   - Celkem vytvořte 3 buckety tak, že objekty stejného entitního typu ukládejte do stejného bucketu
#   - Můžete pracovat s libovolným datovým formátem (např. JSON, text, ...), ale pokaždé přidejte tzv. content headers
# - Vytvořte alespoň 5 smysluplných linků, přičemž mezi nimi rozlišujte 2 různé tagy
#   - Použijte samostatnou hlavičku Link pro každý individuální link
# - Vykonejte 1 požadavek čtení a 1 požadavek aktualizace dat
# - Vykonejte 2 dotazy zahrnující tzv. link walking, přičemž jeden z nich musí zahrnovat alespoň 1 navigační krok a druhý musí obsahovat alespoň 2 navigační kroky
# - Na konci scriptu smažte všechny vytvořené objekty

# ---
# Buckets

# Bucket 1 - Rooms
curl -i -X PUT -H 'Content-Type: application/json' -d '{"number" : 1, "category" : "Single room", "capacity" : 1, "extraCapacity" : 0, "features" : [ "workplace" ], "price" : 150}' http://localhost:10011/buckets/$1_rooms/keys/room1
curl -i -X PUT -H 'Content-Type: application/json' -d '{"number" : 2, "category" : "Double room", "capacity" : 2, "extraCapacity" : 3, "features" : [ "tv", "ocean view" ], "price" : 200}' http://localhost:10011/buckets/$1_rooms/keys/room2
curl -i -X PUT -H 'Content-Type: application/json' -d '{"number" : 3, "category" : "Double room", "capacity" : 2, "extraCapacity" : 3, "features" : [ "tv" ], "price" : 200}' http://localhost:10011/buckets/$1_rooms/keys/room3
curl -i -X PUT -H 'Content-Type: application/json' -d '{"number" : 4, "category" : "Quadruple room", "capacity" : 4, "extraCapacity" : 0, "features" : [ "tv", "ocean view", "terrace" ], "price" : 250}' http://localhost:10011/buckets/$1_rooms/keys/room4
curl -i -X PUT -H 'Content-Type: application/json' -d '{"number" : 5, "category" : "Apartment", "capacity" : 4, "extraCapacity" : 2, "features" : [ "living room", "bar", "workplace", "tv" ], "price" : 400}' http://localhost:10011/buckets/$1_rooms/keys/room5

# Bucket 2 - Customers
curl -i -X PUT -H 'Content-Type: application/json' -d '{"name" : "Jake Gyllenhaal", "birthYear": 1980, "country" : "USA" }' http://localhost:10011/buckets/$1_customers/keys/gyllenhaalj
curl -i -X PUT -H 'Content-Type: application/json' -d '{"name" : "Maggie Gyllenhaal", "birthYear": 1977, "country" : "USA" }' http://localhost:10011/buckets/$1_customers/keys/gyllenhaalm
curl -i -X PUT -H 'Content-Type: application/json' -d '{"name" : "Henry Cavill", "birthYear": 1983, "country" : "GBR" }' http://localhost:10011/buckets/$1_customers/keys/cavill
curl -i -X PUT -H 'Content-Type: application/json' -d '{"name" : "Nicolas Cage", "birthYear": 1964, "country" : "USA" }' http://localhost:10011/buckets/$1_customers/keys/cage
curl -i -X PUT -H 'Content-Type: application/json' -d '{"name" : "Zdeněk Svěrák", "birthYear": 1936, "country" : "CZE" }' http://localhost:10011/buckets/$1_customers/keys/sverak

# Bucket 3 - Reservation
curl -i -X PUT -H 'Content-Type: application/json' -d '{"startDate" : "2019-02-15T15:46:00Z", "endDate" : "2019-02-17T11:00:00Z", "state" : "processed", "notes" : [ "children" ]}' http://localhost:10011/buckets/$1_reservations/keys/res1
curl -i -X PUT -H 'Content-Type: application/json' -d '{"startDate" : "2020-11-13T16:32:00Z", "endDate" : "2020-11-23T11:00:00Z", "state" : "checked-in", "notes" : [ "" ]}' http://localhost:10011/buckets/$1_reservations/keys/res2
curl -i -X PUT -H 'Content-Type: application/json' -d '{"startDate" : "2021-02-15T16:00:00Z", "endDate" : "2021-02-23T15:00:00Z", "state" : "confirmed", "notes" : [ "late check-out" ]}' http://localhost:10011/buckets/$1_reservations/keys/res3
curl -i -X PUT -H 'Content-Type: application/json' -d '{"startDate" : "2020-11-15T17:24:00Z", "endDate" : "2020-11-21T15:00:00Z", "state" : "checked-in", "notes" : [ "late check-out" ]}' http://localhost:10011/buckets/$1_reservations/keys/res4
curl -i -X PUT -H 'Content-Type: application/json' -d '{"startDate" : "2021-10-15T20:00:00Z", "endDate" : "2021-10-19T15:00:00Z", "state" : "confirmed", "notes" : [ "late check-in", "late check-out" ]}' http://localhost:10011/buckets/$1_reservations/keys/res5

# ---
# Links
# Did find a way to use the $1_bucket here - in the link calls it had to be hardcoded.

curl -i -X PUT -H 'Content-Type: application/json' -H 'Link: </buckets/m201_bastianl_reservations/keys/res1>; riaktag="treserved"' http://localhost:10011/buckets/$1_customers/keys/gyllenhaalm
curl -i -X PUT -H 'Content-Type: application/json' -H 'Link: </buckets/m201_bastianl_reservations/keys/res1>; riaktag="treserved"' -H 'Link: </buckets/m201_bastianl_reservations/keys/res2>; riaktag="treserved"' http://localhost:10011/buckets/$1_customers/keys/gyllenhaalj
curl -i -X PUT -H 'Content-Type: application/json' -H 'Link: </buckets/m201_bastianl_reservations/keys/res4>; riaktag="treserved"' -H 'Link: </buckets/m201_bastianl_reservations/keys/res5>; riaktag="treserved"' http://localhost:10011/buckets/$1_customers/keys/cavill
curl -i -X PUT -H 'Content-Type: application/json' -H 'Link: </buckets/m201_bastianl_customers/keys/gyllenhaalj>; riaktag="tassigned"' http://localhost:10011/buckets/$1_rooms/keys/room1
curl -i -X PUT -H 'Content-Type: application/json' -H 'Link: </buckets/m201_bastianl_customers/keys/cavill>; riaktag="tassigned"' http://localhost:10011/buckets/$1_rooms/keys/room5

# ---
# Read

curl -i -X GET http://localhost:10011/buckets/$1_reservations/keys/res2

# ---
# Update

curl -i -X PUT -H 'Content-Type: application/json' -d '{"number" : 4, "category" : "Quadruple room", "capacity" : 4, "extraCapacity" : 1, "features" : [ "tv", "ocean view", "terrace", "minibar" ], "price" : 300}' http://localhost:10011/buckets/$1_rooms/keys/room4

# ---
# Link walking

# Find all reservations of Jake Gyllenhaal
curl -i -X GET http://localhost:10011/buckets/$1_customers/keys/gyllenhaalj/$1_reservations,treserved,1
# Find all reservations of the person that is currently assigned the apartment (room5).
curl -i -X GET http://localhost:10011/buckets/$1_rooms/keys/room5/$1_customers,tassigned,0/$1_reservations,treserved,1

# ---
# Cleanup

# Rooms
curl -i -X DELETE http://localhost:10011/buckets/$1_rooms/keys/room1
curl -i -X DELETE http://localhost:10011/buckets/$1_rooms/keys/room2
curl -i -X DELETE http://localhost:10011/buckets/$1_rooms/keys/room3
curl -i -X DELETE http://localhost:10011/buckets/$1_rooms/keys/room4
curl -i -X DELETE http://localhost:10011/buckets/$1_rooms/keys/room5

# Customers
curl -i -X DELETE http://localhost:10011/buckets/$1_customers/keys/gyllenhaalj
curl -i -X DELETE http://localhost:10011/buckets/$1_customers/keys/gyllenhaalm
curl -i -X DELETE http://localhost:10011/buckets/$1_customers/keys/cavill
curl -i -X DELETE http://localhost:10011/buckets/$1_customers/keys/cage
curl -i -X DELETE http://localhost:10011/buckets/$1_customers/keys/sverak

# Reservations
curl -i -X DELETE http://localhost:10011/buckets/$1_reservations/keys/res1
curl -i -X DELETE http://localhost:10011/buckets/$1_reservations/keys/res2
curl -i -X DELETE http://localhost:10011/buckets/$1_reservations/keys/res3
curl -i -X DELETE http://localhost:10011/buckets/$1_reservations/keys/res4
curl -i -X DELETE http://localhost:10011/buckets/$1_reservations/keys/res5