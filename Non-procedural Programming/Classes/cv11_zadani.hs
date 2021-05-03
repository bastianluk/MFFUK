import Data.List
-- TYPECLASSES A INSTANCE
-- připomenutí:

-- Typové třídy se kterými jsme se víceméně setkali: Eq, Num, Ord, Show, Enum
kolik :: Eq a => a -> [a] -> Int
kolik y = foldr (\x c -> c + (if x == y then 1 else 0)) 0

-- když se podíváme na výstup :info Int, zjistíme, že Int má instanci typové třídy Eq, tudíž lze použít ve funkci kolik

-- také jsme se setkali s deriving, třeba:
data Pozdrav = Ahoj | Cus deriving (Show, Eq)

-- definice třídy Eq v prelude:
-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)  

-- nějaký nový typ, který budeme definovat jako instanci Eq
data TrafficLight = Red | Yellow | Green | Libovolny Int

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

-- všimněte si, že není potřeba definovat /=, protože je zadefinovaný v třídě Eq pomocí ==

-- teď můžeme používat TrafficLight všude, kde je omezení na typovou třídu!
-- například group :: Eq a => [a] -> [[a]]
-- > group [Red, Red, Green, Green, Green]

-- TODO: Zkuste naprogramovat instanci typu TrafficLight pro typovou třídu Show. Je potřeba pouze
--       nadefinovat funkci show :: TrafficLight -> String
instance Show TrafficLight where  
    show Red = "Cervena"
    show Green = "Zelena"
    show Yellow = "Zluta"
    show (Libovolny x) = "Lib " ++ (show x)

emptytree :: Tree a -> Bool
emptytree EmptyTree = True
emptytree _ = False

-- instance typových konstruktorů
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- ? proč nedává smysl definovat ?:
-- instance Eq Mozna where  
--      ...
-- případně:
-- instance Eq Tree where  
--      ...

-- a co nám chybí zde?:
-- instance Eq (Mozna m) where  
--     Proste x == Proste y = x == y  
--     Nic == Nic = True  
--     _ == _ = False  

instance (Eq m) => Eq (Mozna m) where  
    Proste x == Proste y = x == y  
    Nic == Nic = True  
    _ == _ = False  

hloubka :: Tree a -> Int
hloubka EmptyTree = 0
hloubka (Node _ l p) = max (hloubka l) (hloubka p) + 1

-- (Node 1 (Node 0 EmptyTree EmptyTree) EmptyTree) == (Node 1 EmptyTree EmptyTree)

-- TODO a: naprogramujte instanci typu Tree pro třídu Eq, stromy se budou porovnávat podle struktury a podle hodnoty vrcholů
instance (Eq a) => Eq (Tree a) where
    (==) EmptyTree EmptyTree = True
    (==) (Node v1 l p) (Node v2 l2 p2) = v1 == v2 && l == l2 && p == p2
    (==) _ _ = False

-- TODO b: naprogramujte instanci typu Tree pro třídu Eq, stromy se budou porovnávat jen podle struktury
-- instance (Eq m) => Eq (Tree m) where
--     EmptyTree == EmptyTree = True
--     (Node _ l p) == (Node _ a b) = (l == a) && (p == b)
--     _ == _ = False

-- instance Eq Bool where
--     True == True = True
--     False == False = True
--     _ == _ = False

-- typová třída emulující truthy/falsy hodnoty
class YesNo a where
    yesno :: a -> Bool

-- instance typu Int typové třídy YesNo
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

-- Vysvětlení proč nelze zadefinovat následující instanci:
-- https://stackoverflow.com/questions/27566373/exception-for-the-instance-num-a-yesno-a-where-code-row
-- instance (Num a) => YesNo a where
--     yesno x | x == fromIntegral 0 = False
--             | otherwise = True

-- TODO: Doplňte instance pro typy Bool, Tree, [a]
instance YesNo Bool where
    yesno = id

instance YesNo (Tree a) where
    yesno EmptyTree = False  
    yesno _ = True

instance YesNo [a] where
    yesno [] = False  
    yesno _ = True


-- typová třída Functor
-- pro typy, na které můžeme použít map. Tedy kontejnery (třeba seznam)
-- class Functor f where  
--     fmap :: (a -> b) -> f a -> f b  

data Mozna a = Proste a | Nic deriving (Show) -- překlad typu Maybe, se kterým se ještě setkáme

-- takto vypadá instance pro pole, všimněte si typového konstruktoru pole, ještě jsme se s ním nesetkali:
-- instance Functor [] where  
--     fmap = map  

-- TODO: Napište instanci Funktoru pro typové konstruktory Mozna a Tree
-- příklad: > fmap negate (Proste 3)
-- Proste (-3)

instance Functor Mozna where
    fmap f Nic = Nic
    fmap f (Proste x) = Proste (f x)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node v l p) = Node (f v) (fmap f l) (fmap f p)

-- Co kdybychom měli typ definovaný:
-- data Either a b = Left a | Right b
-- a chtěli z něj udělat instanci třídy Funktor?
-- mohli bychom částečně aplikovat typový konstruktor:
-- instance Functor (Either a) where  
    -- fmap f (Right x) = Right (f x)  
    -- fmap f (Left x) = Left x  

-- Všimněte si, že typové konstruktory přijímají jiné typy jako parametry a ve výsledku vytvoří konkrétní typy.
-- To je trochu jako když funkce přijímají nějaké hodnoty aby vytvořili jinou hodnotu.
-- Taky jsme teď viděli, že typové konstruktory lze částečně aplikovat
-- Každá hodnota má svůj typ, stejně tak každý typ má svůj kind (druh?)
-- prohlédnout si kind nějakého typu můžete pomocí :k Int

-- TODO: Napište instanci Foldable pro typový konstruktor Tree
-- class Foldable (t :: * -> *) where
--     ...
--     foldr :: (a -> b -> b) -> b -> t a -> b
--     ...


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
