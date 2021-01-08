#!/bin/bash

# bastian - theme: hotel

# Zadání:
#   Vyjádřete alespoň 4 různé složené dotazy (angl. compound query), přičemž použijte alespoň 4 z následujících klauzulí: bool, boosting, constant_score, dis_max, function_score
#   Vyjádřete alespoň jeden agregační dotaz
#   Nezapomeňte na konci scriptu vymazat všechny vytvořené indexy
# Požadavky:
#   Indexy pojmenovávejte na základě této jmenné konvence: m201_login_index, např. m201_student_animals
#   Uvnitř scriptu nepřistupujte k indexům přímo, nýbrž použijte $1_index
#   $1 je parametrem scriptu, který je předán během jeho volání a který odpovídá prefixu m201_login
#   V komentáři v přirozeném jazyce popište, co Vaše dotazy dělají a za jakým účelem používáte vybrané klauzule, a to pomocí echo "text" komentářů
#   Ujistěte se, že Váš script je vykonavatelný, tzn. má přiřazena práva executable (chmod +x script.sh)
#   Váš script musí být opakovatelně vykonavatelný, a to bez selhání
# Odevzdávané soubory:
#   script.sh Bash skript, který umožňuje vykonat veškeré HTTP požadavky nad Elasticsearch
#   data.txt: Definice dokumentů, které indexujete pomocí bulk
# Vykonání:
#   Vykonejte následující shell přikaz: ./script.sh $IndexPrefix
#   $IndexPrefix zastupuje prefix jmen Vašich indexů, např. m201_login\

# MAPPING
curl -X PUT "localhost:9200/$1_reservation?pretty" -H 'Content-Type: application/json' -d "{
  \"mappings\": {
    \"properties\": {
      \"customer\": {
        \"properties\" : { \"name\" : {\"type\" : \"text\", \"index\": true }, \"year\": { \"type\": \"integer\" }, \"country\" : {\"type\" : \"text\"} }
      },
      \"startDate\": { \"type\": \"integer\", \"index\": true },
      \"endDate\": { \"type\": \"integer\", \"index\": true },
      \"rooms\": { \"type\": \"keyword\" },
      \"state\": { \"type\": \"text\" },
      \"price\": { \"type\": \"integer\" }
    } }
}"


# LOAD
curl -s -H "Content-Type: application/x-ndjson" -XPOST localhost:9200/_bulk --data-binary "@data.txt";


# QUERIES
# a

# b

# c

# d



# CLEANUP
curl -X DELETE "localhost:9200/$1_*"