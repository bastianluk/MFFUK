import Data.List
type Castka = Integer

data Operace
  = Prihlaseni
  | Odhlaseni
  | Vyber Castka
  | Pripis Castka
  deriving (Show, Read, Eq)

type Cas = Integer

type Uzivatel = String

data Zaznam =
  Zaznam Cas
         Uzivatel
         Operace
  deriving (Show, Read, Eq)

type Zaznamy = [Zaznam]

main = do
  log <- (map read . lines <$> readFile "banka.log") :: IO [Zaznam] --nacteni a rozparsovani logu
  let result cmt f --pomocna funkce na vypisovani vysledku
       = do
        putStrLn (cmt ++ ":")
        print (f log)
        putChar '\n'
  {- pocitani a vypisovani vysledku zacina zde -}
  result
    "DEMO1 -- jmeno prvniho uzivatele v souboru se smichanymi zaznamy"
    demoPrvniZaznam
  result
    "DEMO2 -- pocet zaznamu se jmenem Marie"
    demoPocetMarie
  result "Seznam uzivatelu serazenych podle abecedy" serazeniUzivatele
  result "Casy top 10 nejvetsich vyberu" top10vyber
  result "Jmena uzivatelu 10 nejmensich pripisu" top10pripis
  result "Nejaktivnejsi uzivatel" topUzivatel
  result "Uzivatel ktery vydelal nejvic penez" topPrirustek
  result "BONUS: Prumerna vybrana castka uzivatelu zacinajicich od J" prumerVyberuJ
  result
    "BONUS: Uzivatel s nejdelsi posloupnosti akci nerusenou v logu jinymi uzivateli"
    nejdelsiSingleRun

-- příklad 1: Jméno uživatele prvního záznamu v logu
demoPrvniZaznam :: Zaznamy -> Uzivatel
demoPrvniZaznam ((Zaznam _ jm _):_) = jm

-- příklad 2: Počet záznamů se jménem Marie
demoPocetMarie :: Zaznamy -> Int
demoPocetMarie = length . filter uzivatelMarie
  where
    uzivatelMarie (Zaznam _ "Marie" _) = True
    uzivatelMarie _ = False
-- ekvivalentně:
-- demoPocetMarie zaznamy = length $ filter uzivatelMarie zaznamy
-- nebo:
-- demoPocetMarie zaznamy = length (filter uzivatelMarie zaznamy)

{- Ukol zacina tady. Misto `undefined` dodejte definice funkci, ktere z logu
 - vytahnou pozadovany vysledek. -}

getName :: Zaznam -> String
getName (Zaznam _ name _) = name

getTime :: Zaznam -> Cas
getTime (Zaznam time _ _) = time

getAmount :: Zaznam -> Castka
getAmount (Zaznam _ _ (Vyber value)) = value
getAmount (Zaznam _ _ (Pripis value)) = value

negativeAmount :: Zaznam -> Castka
negativeAmount (Zaznam _ _ (Vyber value)) = -value
negativeAmount (Zaznam _ _ (Pripis value)) = -value

-- Seznam uživatelů (bez duplicit), seřazený podle abecedy
serazeniUzivatele :: Zaznamy -> [Uzivatel]
-- get the names, take unique entries only
serazeniUzivatele = nub . sort . map getName

-- Časy deseti největších výběrů
top10vyber :: Zaznamy -> [Cas]
-- find withdrawals only, find the biggest ones and take just their time
top10vyber = map getTime . take 10 . sortOn negativeAmount . withdrawals

withdrawals :: Zaznamy -> Zaznamy
withdrawals = filter withdrawal
  where
    withdrawal (Zaznam _ _ (Vyber _)) = True
    withdrawal _ = False

-- Jména uživatelů, kterým přišlo deset nejmenších přípisů (bez opakování jmen)
top10pripis :: Zaznamy -> [Uzivatel]
-- pick just the income transactions, sort by the amount, pick the names and take the first 5 unique ones
top10pripis = take 5 . nub . map getName . sortOn getAmount . incomes

incomes :: Zaznamy -> Zaznamy
incomes = filter income
  where
    income (Zaznam _ _ (Pripis _)) = True
    income _ = False

-- Jméno uživatele, který je nejaktivnější (tj. má v logu nejvíc záznamů)
topUzivatel :: Zaznamy -> Uzivatel
-- smart grouping by name and pick the biggest group
topUzivatel = groupedName . last . sortOn length . groupByName

groupByName :: Zaznamy -> [Zaznamy]
groupByName = groupBy (\x y -> getName x == getName y) . sortOn getName

groupedName :: Zaznamy -> Uzivatel
groupedName = getName . head

-- Jméno uživatele, kterému na účtu přibylo nejvíc peněz (tj. má maximální součet příjmů mínus součet výdajů)
topPrirustek :: Zaznamy -> Uzivatel
-- group by name and summarize his transactions, take the biggest balance
topPrirustek = fst . last . sortOn snd . summarize . groupByName
-- maximumBy could be used

summarize :: [Zaznamy] -> [(Uzivatel, Castka)]
summarize = map summarizeInner

summarizeInner :: Zaznamy -> (Uzivatel, Castka)
summarizeInner list = (summarizeInnerFirst list, summarizeInnerSecond list)
summarizeInnerFirst:: Zaznamy -> Uzivatel
summarizeInnerFirst = groupedName
summarizeInnerSecond:: Zaznamy -> Castka
summarizeInnerSecond = sumCastka . map castkaOfZaznam

sumCastka :: [Castka] -> Castka
sumCastka = foldl (+) 0

castkaOfZaznam :: Zaznam -> Castka
castkaOfZaznam (Zaznam _ _ (Vyber amount)) = -amount
castkaOfZaznam (Zaznam _ _ (Pripis amount)) = amount
castkaOfZaznam _ = 0

-- BONUS: Průměrná částka (oříznutá na celé číslo), kterou vybrali uživatelé začínající od J
prumerVyberuJ :: Zaznamy -> Castka
-- filter by name, only withdrawals and calculate the avg
prumerVyberuJ = avgCastka . withdrawals . nameStartsWithJ

nameStartsWithJ :: Zaznamy -> Zaznamy
nameStartsWithJ = filter namePrefixTest
  where
    namePrefixTest (Zaznam _ name _) = isPrefixJ name
isPrefixJ (c:_) = c == 'J'
isPrefixJ _ = False

avgCastka :: Zaznamy -> Castka
avgCastka list = div (sumCastka (map getAmount list)) (genericLength list)

-- BONUS: Jméno uživatele, který provedl nejvíc akcí za sebou bez toho, aby jakýkoliv jiný uživatel cokoliv udělal (tj. po seřazení logu podle času bude mít “nejvíc řádků po sobě”)
nejdelsiSingleRun :: Zaznamy -> Uzivatel
-- sort by time, group the records, find longest and pick just the name
nejdelsiSingleRun = groupedName . last . sortOn length . groupBy (\x y -> getName x == getName y) . sortOn getTime