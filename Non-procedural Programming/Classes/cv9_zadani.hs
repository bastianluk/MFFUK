-- RANGES + LIST COMPREHENSIONS
-- TODO: násobky devíti od čísla 90 pozpátku k nule (v Haskellu zapsáno pomocí jednoho výrazu o 10 znacích)

-- TODO: mensiNez n xs -- vrátí seznam, který obsahuje prvky vybrané z xs menší než n (místo filter použijte list comprehension)

-- TODO: removeNonUppercase str -- vrátí string str bez znaků, které nejsou uppercase (pro seznam velkých písmen použijte ['A'..'Z'], pro test prvku v seznamu funkci elem)

-- TODO: dvojice n -- seznam všech dvojic přirozených čísel do n

-- TODO: pyth n -- vrátí seznam Pythagorejských trojic se stranou menší nebo rovno n (bez opakování)
-- > pyth 20
-- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]

-- concat napsané pomocí list comprehensions:
-- concat xss = [x|xs<-xss, x<-xs]

-- NEKONEČNÉ SEZNAMY

-- TODO: seznam obsahující všechna lichá čísla, zapsáno pomocí range (můžete zkontrolovat pomocí take)
-- TODO: seznam obsahující všechna lichá čísla, zapsáno pomocí rekurzivní definice

-- poznámka: existují funkce repeat a cycle, které vytvoří 
--           nekonečný seznam pomocí opakování jednoho čísla (repeat) a nebo seznamu (cycle)
--           pokud si nejste jisti, jak byste je naimplementovali, zkuste to (na základě předchozího TODO)

-- TODO: fibonacciho čísla pomocí rekurzivní definice funkce o dvou argumentech (poslední dvě fib. čísla)
-- TODO: fibonacciho čísla pomocí funkcí zipWith + tail

-- VLASTNÍ TYPY
-- viz http://learnyouahaskell.com/making-our-own-types-and-typeclasses
data MyBool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  
-- Circle x y radius, Rectangle x1 y1 x2 y2

-- TODO: Napište funkci surface která vypočte plochu Shape
-- surface :: Shape -> Float  

-- TODO: Použijte tuto funkci a převeďte seznam [Circle 0 0 4, Circle 1 1 3, Rectangle 1 1 3 4] na seznam odpovídajících ploch

-- TODO: funkce isCircle :: Shape -> Bool, která určí, jestli je Shape v argumentu kruh

-- TODO: Vlastní typ Complex, místo pevně určených typů bude parametrizovaný

-- TODO: funkce real a imag, které vrátí příslušnou část komplexního čísla

-- TODO: Implementace násobení komplexních čísel

-- Record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- TODO: Complex pomocí record syntax

-- Type synonyms
type MyString = [Char]  

-- Parametrizované typy
-- funkce lookup key list, která *možná* najde hodnotu na základě klíče v seznamu dvojic (klíč, hodnota)
-- > lookup "Martina" [("Marie", 100), ("Marek", 392), ("Martina", 1929)] 
-- Just 1929
-- > lookup "Honza" [("Marie", 100), ("Marek", 392), ("Martina", 1929)] 
-- Nothing
my_lookup :: Eq a => a -> [(a,b)] -> Maybe b
my_lookup key [] = Nothing  
my_lookup key ((k,v):xs)
    | key == k  = Just v
    | otherwise = my_lookup key xs  

-- TODO: Vlastní typ Mozna, který bude fungovat stejně jako Maybe

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
        deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Rekurzivní typy

-- TODO: Nadefinujte datový typ pro binární strom s hodnotami ve vrcholech a prázdnými listy
data Tree a = Node (Tree a) a (Tree a) | Leaf
    deriving (Show)
-- TODO: insertT, lookupT pro náš strom
insertS :: Ord a => Tree a -> a -> Tree a
insertS Leaf x = Node Leaf x Leaf
insertS (Node l v r) x
    | x < v     = Node (insertS l x) v r
    | otherwise = Node l v (insertS r x)

-- TODO: treeToList