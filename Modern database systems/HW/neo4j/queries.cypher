// bastianl - theme - hotel

// Zadání:
//  Vložte realistické vrcholy (uzly) a hrany (vztahy) do Vaší embedded Neo4j databáze
//  Použijte jeden CREATE výraz pro tento účel
//      Vložte celkem alespoň 10 uzlů pro entity alespoň 2 různých typů (tzn. různé labels)
//      Vložte celkem alespoň 15 vztahů, a to alespoň 2 různých vztahových typů
//      Definujte atributy u uzlů i vztahů
//      Ke každému vloženému uzlu přidejte uživatelem definovaný identifikátor
//  Vyjádřete 5 Cypher dotazů
//      Alespoň 1x použijte všechny následující (sub)klauzule: MATCH, OPTIONAL MATCH, RETURN, WITH, WHERE a ORDER BY
//      Použijte agregaci alespoň v jednom dotazu
// Požadavky:
//  Popište význam Vašich Cypher výrazů v přirozeném jazyce (pomocí // komentářů)
// Odevzdávané soubory:
//  queries.cypher: Textový soubor obsahující sekvenci Cypher výrazů (včetně CREATE)
// Vykonání:
//  Vykonejte následující shell přikaz: cat $ScriptFile | neo4j-shell --path $DatabasePath --non-interactive
//  $DatabasePath je cestou k adresáři, který obsahuje Neo4j embedded databá


CREATE
    (c1:CUSTOMER { id: "vratnelahve", title: "Vratne lahve", year: 2006 }),
    (c2:CUSTOMER { id: "samotari", title: "Samotari", year: 2000 }),
    (c3:CUSTOMER { id: "medvidek", title: "Medvidek", year: 2007 }),
    (c4:CUSTOMER { id: "stesti", title: "Stesti", year: 2005 }),
    (c5:CUSTOMER { id: "stesti", title: "Stesti", year: 2005 }),
    (r1:ROOM { id: "trojan", name: "Ivan Trojan", year: 1964 }),
    (r2:ROOM { id: "machacek", name: "Jiri Machacek", year: 1966 }),
    (r3:ROOM { id: "schneiderova", name: "Jitka Schneiderova", year: 1973 }),
    (r4:ROOM { id: "sverak", name: "Zdenek Sverak", year: 1936 }),
    (r5:ROOM { id: "sverak", name: "Zdenek Sverak", year: 1936 }),
    (res1:RESERVATION { id: "trojan", name: "Ivan Trojan", year: 1964 }),
    (res2:RESERVATION { id: "machacek", name: "Jiri Machacek", year: 1966 }),
    (res3:RESERVATION { id: "schneiderova", name: "Jitka Schneiderova", year: 1973 }),
    (res4:RESERVATION { id: "sverak", name: "Zdenek Sverak", year: 1936 }),
    (res5:RESERVATION { id: "sverak", name: "Zdenek Sverak", year: 1936 }),
    (res1)-[a1:ASSIGNED { occupancy: 3 }]->(r1),
    (res1)-[a2:ASSIGNED { occupancy: 3  }]->(r4),
    (res1)-[a3:ASSIGNED { occupancy: 3  }]->(r1),
    (res1)-[a4:ASSIGNED { occupancy: 3  }]->(r2),
    (res1)-[a5:ASSIGNED { occupancy: 3  }]->(r3),
    (res1)-[a6:ASSIGNED { occupancy: 3  }]->(r1),
    (res1)-[a7:ASSIGNED { occupancy: 3 , chidlren: 1 }]->(r1),
    (res1)-[a8:ASSIGNED { occupancy: 3  }]->(r1),
    (res1)-[a9:ASSIGNED { occupancy: 3  }]->(r1),
    (res1)-[a10:ASSIGNED { occupancy: 3  }]->(r1),
    (res2)-[o1:ORDEREDBY { price: 3 }]->(c1),
    (res2)-[o2:ORDEREDBY { price: 3 }]->(c1),
    (res2)-[o3:ORDEREDBY { price: 3 }]->(c1),
    (res2)-[o4:ORDEREDBY { price: 3 }]->(c1);
    (res2)-[o5:ORDEREDBY { price: 3 }]->(c1);