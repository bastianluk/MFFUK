-- RANGES + LIST COMPREHENSIONS
-- TODO: násobky devíti od čísla 90 pozpátku k nule (v Haskellu zapsáno pomocí jednoho výrazu o 10 znacích)

-- TODO: mensiNez n xs -- vrátí seznam, který obsahuje prvky vybrané z xs menší než n (místo filter použijte list comprehension)
mensiNez n xs = [x | x <- xs, x < n]

-- TODO: removeNonUppercase str -- vrátí string str bez znaků, které nejsou uppercase (pro seznam velkých písmen použijte ['A'..'Z'], pro test prvku v seznamu funkci elem)
-- removeNonUppercase str = filter (\x -> elem x ['A'..'Z']) str
removeNonUppercase str = [x | x<-str, elem x ['A'..'Z']]

-- TODO: dvojice n -- seznam všech dvojic přirozených čísel do n
dvojice n = [(a, b) | a <- cisla, b <- cisla]
    where cisla = [1..n]
-- TODO: pyth n -- vrátí seznam Pythagorejských trojic se stranou menší nebo rovno n (bez opakování)
-- > pyth 20
-- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]
pyth n = [ (x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], z*z == y*y + x*x]

isempty [] = True
isempty (_:_) = False

-- concat napsané pomocí list comprehensions:
-- [[1,2,3], [3,8,3], [1,2]]

-- my_concat [[[1,2],[3,4]],[[5,6],[7,8]]]
-- my_concat xsss = [x | xss<-xsss, xs<-xss, x<-xs]

-- concat xss = [x|xs<-xss, x<-xs]

-- NEKONEČNÉ SEZNAMY

-- TODO: seznam obsahující všechna lichá čísla, zapsáno pomocí range (můžete zkontrolovat pomocí take)

-- TODO: seznam obsahující všechna lichá čísla, zapsáno pomocí rekurzivní definice
jednicky = 1 : jednicky
lichy = 1 : (map (+2) lichy)

-- poznámka: existují funkce repeat a cycle, které vytvoří 
--           nekonečný seznam pomocí opakování jednoho čísla (repeat) a nebo seznamu (cycle)
--           pokud si nejste jisti, jak byste je naimplementovali, zkuste to (na základě předchozího TODO)

-- TODO: fibonacciho čísla pomocí rekurzivní definice funkce o dvou argumentech (poslední dvě fib. čísla)
-- fib a b = (a+b):(fib b (a+b))
-- fib a b = a:(fib b (a+b))
-- TODO: fibonacciho čísla pomocí funkcí zipWith + tail
-- fib = 1:1:(zipWith (+) fib (tail fib))

-- VLASTNÍ TYPY

-- viz http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- data MyBool = MyFalse | MyTrue

data Shape = Circle Double Double Double | Rectangle Double Double Double Double deriving (Show)
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  

-- (1, 1, 30.3) :: (Float, Float, Float)
area :: Shape -> Double
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs (y2-y1))*(abs (x2 - x1))

-- Circle x y radius, Rectangle x1 y1 x2 y2

-- TODO: Napište funkci surface která vypočte plochu Shape
-- surface :: Shape -> Double  
-- TODO: Použijte tuto funkci a převeďte seznam [Circle 0 0 4, Circle 1 1 3, Rectangle 1 1 3 4] na seznam odpovídajících ploch
toArea :: [Shape] -> [Double]
toArea xs = map (area) xs

-- TODO: funkce isCircle :: Shape -> Bool, která určí, jestli je Shape v argumentu kruh
-- isCircle :: Shape -> Bool
-- isCircle (Circle _ _ _) = True
-- isCircle _ = False

-- TODO: Vlastní typ Complex, místo pevně určených typů bude parametrizovaný
-- data Complex a = Complex a a     deriving (Show)
-- TODO: funkce real a imag, které vrátí příslušnou část komplexního čísla

-- TODO: Implementace násobení komplexních čísel

-- Record syntax
-- data Car = Car String String Int
-- data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- TODO: Complex pomocí record syntax
data Complex a = C {real :: a, imag :: a}     deriving (Show)

multComplexTuple :: ComplexTuple -> ComplexTuple -> ComplexTuple
multComplexTuple (r1, i1) (r2, i2) = (r3, i3)
    where
        r3 = r1*r2 - i1*i2
        i3 = r1*i2 + r2*i1

multComplex :: Num a => Complex a -> Complex a -> Complex a
multComplex (C r1 i1) (C r2 i2) = C (r1*r2 - i1*i2) (r1*i2 + r2*i1)

-- Type synonyms
type ComplexTuple = (Double, Double)

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

-- 
data List a = Nil | Cons a (List a)   deriving (Show)
prikladSeznam = Cons 1 (Cons 2 (Cons 3 Nil))

my_head :: List a -> a
my_head (Cons x _) = x

-- TODO: Nadefinujte datový typ pro binární strom s hodnotami ve vrcholech a prázdnými listy
data Tree a = Node (Tree a) a (Tree a) | Leaf
    deriving (Show)
-- TODO: insertT, lookupT pro náš strom
insertS :: Ord a => Tree a -> a -> Tree a
insertS Leaf x = Node Leaf x Leaf
insertS (Node l v r) x
    | x < v     = Node (insertS l x) v r
    | otherwise = Node l v (insertS r x)

-- lookup :: Tree a -> a -> Bool
-- TODO: treeToList
