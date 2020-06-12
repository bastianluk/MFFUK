import Data.List
-- TYPECLASSES A INSTANCE
-- připomenutí:

-- Typové třídy se kterými jsme se víceméně setkali: Eq, Num, Ord, Show, Enum
kolik :: Eq a => a -> [a] -> Int
kolik y = foldr (\x c -> c + (if x == y then 1 else 0)) 0

-- když se podíváme na výstup :info Int, zjistíme, že Int má instanci typové třídy Eq, tudíž lze použít ve funkci kolik

-- také jsme se setkali s deriving, třeba:
data Pozdrav = Ahoj | Cus deriving (Show)

-- definice třídy Eq v prelude:
-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)  

-- nějaký nový typ, který budeme definovat jako instanci Eq
data TrafficLight = Red | Yellow | Green

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

-- instance typových konstruktorů
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
data Mozna a = Proste a | Nic -- překlad typu Maybe, se kterým se ještě setkáme
-- 

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

-- TODO a: naprogramujte instanci typu Tree pro třídu Eq, stromy se budou porovnávat podle struktury a podle hodnoty vrcholů
-- TODO b: naprogramujte instanci typu Tree pro třídu Eq, stromy se budou porovnávat jen podle struktury

-- typová třída emulující truthy/falsy hodnoty
class YesNo a where
    yesno :: a -> Bool

-- instance typu Int typové třídy YesNo
instance YesNo Int where
    yesno 0 = False  
    yesno _ = True

-- TODO: Doplňte instance pro typy Bool, Tree, [a]

-- typová třída Funktor
-- pro typy, na které můžeme použít map. Tedy kontejnery (třeba seznam)
-- class Functor f where  
--     fmap :: (a -> b) -> f a -> f b  

-- takto vypadá instance pro pole, všimněte si typového konstruktoru pole, ještě jsme se s ním nesetkali:
-- instance Functor [] where  
--     fmap = map  

-- TODO: Napište instanci Funktoru pro typové konstruktory Mozna a Tree
-- příklad: > fmap negate (Proste 3)
-- Proste (-3)

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
