import Data.List

-- OPAKOVÁNÍ:
-- TODO: napište funkci rle, která přijímá seznam a komprimuje ho pomocí run-length encoding (posloupnosti stejných znaků převede na znak + počet jeho opakování)
-- `rle xs` tedy vezme seznam xs a převede ho na kratší seznam dvojic. Dvojice obsahuje délku posloupnosti a znak, který se opakuje. (použijte group + map)
-- > rle [1,1,1,2,2,3,2,2,2,1]
-- [(3,1),(2,2),(1,3),(3,2),(1,1)] -- čteme: 3x za sebou jednička, 2x za sebou 2, 1x za sebou 3, atd..
-- rle :: Eq a => [a] -> [(Int, a)]

-- Operátory $ a .

-- definice $: aplikace funkcí s nízkou prioritou
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x
-- příklad použití:
-- soucetCtvercuMensichNezSto = sum (takeWhile (<100) (map (^2) [1..]))
-- soucetCtvercuMensichNezSto = sum $ takeWhile (<100) $ map (^2) [1..]
-- TODO: přepište rle pomocí ($)

-- definice .: skládání funkcí
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
-- f . g = \x -> f (g x)
-- příklad použití:
-- absurdniVypocet x = ceiling (negate (tan (cos (max 50 x)))) 
-- absurdniVypocet = ceiling . negate . tan . cos . max 50
-- TODO: přepište rle pomocí (.) - skládání funkcí

-- OPAKOVÁNÍ:
-- TODO: definujte typ IntX (pomocí data IntX = ....), který rozšiřuje typ Int a přidává k němu pozitivní a negativní nekonečno. 
-- jako názvy datových konstruktorů použijte NegInf, PosInf, Num (poslední jmenovaný konstruktor bude mít navíc jeden argument)
-- můžete zkusit nadefinovat i polymorfní NumX.
-- TODO: pak nadefinujte funkce less a greater, které budou správně porovnávat dvě vstupní hodnoty typu IntX

-- TODO: ze seznamu intxs odfiltrujte všechna nekonečna a převeďte je na normální Int:
intxs = [Num 3, Num 10, NegInf, PosInf, Num 392, NegInf]
-- a) pomocí filter a map
-- b) pomocí list comprehensions (zápis `x <- xs` umí patternmatchovat!)

-- Rekurzivní typy
-- použijte následující definici stromu (nebo klidně svoji vlastní)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 
-- pro testování můžete použít binární vyhledávácí strom testTree:
testTree = Node 10 (Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)) (Node 20 EmptyTree EmptyTree)

-- implementujte včetně specifikace typů funkce (někdy je třeba přidat type constraint, třeba `Ord a =>`):

-- TODO: funkci treeDepth, která spočítá hloubku stromu
-- > treeDepth testTree
-- 3

-- TODO: funkci treeToList, která převede strom na seznam (infix)
-- > treeToList testTree
-- [3,5,7,10,20]

-- TODO: funkci treeInsert, která do vstupního *binárního vyhledávacího* stromu přidá prvek na místo listu a vrátí změněný strom
-- > treeInsert testTree 25
-- Node 10 (Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)) (Node 20 EmptyTree (Node 25 EmptyTree EmptyTree))

-- připomenutí: foldr
-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs) 
-- na funkci foldr lze hledět tak, že nahrazuje konstruktor seznamu : jinou funkcí a konec seznamu [] jinou hodnotou:
-- foldr (+) 0 (1:(2:(3:[]))) = 
            --  1+(2+(3+0))
-- záměrně jsem zde rozepsal seznam [1,2,3] na (1:(2:(3:[]))), což je ekvivalentní zápis

-- foldl naopak funguje obráceně:
-- foldl (+) 0 (1:(2:(3:[]))) = 
            -- ((0+1)+2)+3

-- TODO: zkuste rozmyslet, co vrátí tyto výrazy, pak to zkontrolujte v GHCI:
-- foldr (-) 0 [4, 3, 2, 1]
-- foldl (-) 0 [4, 3, 2, 1]

-- TODO: naprogramujte funkci listToTree, která převede seznam na strom pomocí postupného přidávání (použijte treeInsert + fold)

-- TODO: naprogramujte fold pro stromy (bude fungovat jako foldr), který:
--       jako 1. argument) přijímá funkci se třemi argumenty, první je hodnota v aktuálním vrcholu, druhý a třetí jsou akumulátory s výsledky foldu podstromu. 
--       jako 2. argument) přijímá hodnotu, kterou nahradí listy stromu
--       jako 3. argument) samotný strom, který chceme transformovat pomocí fold

-- TODO: použijte fold pro součet všech prvků stromu
-- TODO: použijte fold pro převod stromu na seznam
-- TODO: použijte fold pro implementaci treeMap

-- TYPECLASSES A INSTANCE

data Pravdivost = Ano | Ne

{- pouziti: Ano se vypise jako Jo diky pretizenemu show.
 - 'show' je jinak zcela typicka overloadovana funkce -- pro kazdy datovy typ
 - funguje uplne jinak. -}
instance Show Pravdivost where
  show Ano = "Jo"
  show Ne = "Nein"

{- druhá definice stromů, tentokrát bez deriving!
 - zakomentujte tu první -}
-- data Tree a = EmptyTree | Node a (Tree a) (Tree a)
{- Pokud strom obsahuje neco show-ovatelneho, tak jde taky show-ovat -}
-- instance Show a => Show (Tree a) where
--   show EmptyTree = "{}"
--   show (Node value left right) =
--     "{" ++ show left ++ " " ++ show value ++ " " ++ show right ++ "}"

-- nadefinujeme si alternativní Eq, která stromy porovnává pouze podle jejich tvaru
-- instance Eq (Tree a) where
--     EmptyTree == EmptyTree = True
--     (Node _ l1 r1) == (Node _ l2 r2) = l1 == l2 && r1 == r2
--     _ == _ = False

{- Strom je kontejner, a jde na nej pouzivat fmap (coz je genericky map)
 - priklad:
    *Main> fmap (reverse.show.(*100)) testTree
    {{{{} "003" {}} "005" {{} "007" {}}} "0001" {{} "0002" {}}}

 -  implementace typeclass Functor:
 -  class Functor f where
 -     fmap :: (a -> b) -> f a -> f b
 -     (<$) :: a -> f b -> f a
 -}

-- instance Functor Tree where
--     fmap _ EmptyTree = EmptyTree
--     fmap f (Node value left right) = Node (f value) (fmap f left) (fmap f right)


{- Typova trida na vsechny slepitelne veci -}
class Slep a where
  slep :: a -> a -> a

{- Lepit k sobe jdou integery (scitanim) -}
instance Slep Integer where
  slep = (+)

{- Lepit k sobe jdou i seznamy (spojovanim seznamu) -}
instance Slep [a] where
  slep = (++)

{- Lepit k sobe jdou i Booly (andem) -}
instance Slep Bool where
  slep = (&&)

{- pouziti:
 -  slep 5 7 == 12
 -  slep [5] [7,8] == [5,7,8]
 -}

{- Typova trida na integery modulo 7. "newtype" je tosame jako "data", ale
 - funguje to jen na data ktere obsahuji presne jednu moznost s presne jednim
 - datovym prvkem, a v runtime to je rychlejsi protoze v prubehu kompilace se
 - newtype "odmaze". -}
newtype I7 =
  I7 Int
  deriving (Show)

{- Data typu I7 jde pouzivat jako cisla -}
instance Num I7 where
  I7 a + I7 b = I7 $ (a + b) `mod` 7
  I7 a - I7 b = I7 $ (a - b) `mod` 7
  I7 a * I7 b = I7 $ (a * b) `mod` 7
  negate (I7 a) = I7 $ negate a `mod` 7
  abs = id
  signum (I7 0) = I7 0
  signum _ = I7 1
  fromInteger a = I7 $ fromInteger a `mod` 7

{- Priklad: I7 5 + 4 * 2 + 1 == I7 0 -}
