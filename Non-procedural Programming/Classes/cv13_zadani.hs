import System.Random
import Data.Char
-- Naše oblíbená definice stromu
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- Typeclass Functor
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- TODO: Co se stane?
--       > (*2) <$> [1, 2, 3]
--       > fmap (*2) Nothing
--       > (*2) <$> (Just 10)
--       > fmap (*2) []
--       > fmap (*2) (Left 2)
--       > fmap (*2) (Right 4)

-- díky fmap můžeme aplikovat funkci uvnitř funktoru, aniž bychom museli neustále odchytávat případy, kdy je funktor prázdný

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- Typeclass Applicative = Aplikativní funktory
-- Co kdybych chtěl použít fmap s funkcemi více parametrů?
-- něco jako fmap2 (*) (Just 3) (Just 4) = 12
--           fmap2 (*) (Just 9) Nothing = Nothing

-- tak nejdřív můžeme udělat fmap (*) (Just 3), posud dobrý. Máme Just (*3) (a nebo Nothing, kdyby výpočet nevyšel)
-- ale teď bychom potřebovali fmap (Just (*3)) (Just 4) .... To už s normálním fmap nejde

-- class (Functor f) => Applicative f where  
--     pure :: a -> f a  
--     (<*>) :: f (a -> b) -> f a -> f b  

-- interpretace funkcí podle jejich typu:
-- `pure` bere a a vrací f a, tedy vezme nějakou hodnotu a zabalí ji v nějakém aplikativním funktoru.
-- `<*>` bere funktor s funkcí uvnitř, vytáhne funkci, zavolá ji na vnitřek dalšího funktoru a vrátí funktor s jinou hodnotou.
    -- porovnejte typ <*> a <$>

-- příklad instance pro Maybe:
-- instance Applicative Maybe where  
--     pure = Just  
--     Nothing <*> _ = Nothing  
--     (Just f) <*> something = fmap f something  

-- a příklady příkazů:
-- ghci> Just (+3) <*> Just 9  
-- Just 12  
-- ghci> pure (+3) <*> Just 10  
-- Just 13  
-- ghci> pure (+3) <*> Just 9  
-- Just 12  
-- ghci> Just (++"hahah") <*> Nothing  
-- Nothing  
-- ghci> Nothing <*> Just "woot"  
-- Nothing  
-- ghci> pure (+) <*> Just 3 <*> Just 5  
-- Just 8  
-- ghci> pure (+) <*> Just 3 <*> Nothing  
-- Nothing  
-- ghci> pure (+) <*> Nothing <*> Just 5  
-- Nothing

-- aplikativní funktor nám umožňuje dívat se na některé datové typy (třeba Maybe) jako na hodnoty s nějakým "kontextem"/obalem 
-- a používat na ně libovolné funkce, aniž bychom tenhle kontext narušili.

-- TODO: naprogramujte pomocí <$> a <*> (a nebo pomocí pure a <*>) funkci:
--       liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
--       která "pozvedne" funkci dvou argumentů (a -> b -> c) tak, aby fungovala
--        s argumenty v kontejnerech `f` a vracela výsledek zabalený také v `f`
-- > liftA2 (+) (Just 2) (Just 3)
-- Just 5

-- mohli bychom mít také aplikativní funktor Result, který bude uchovávat hodnotu a případné chyby (variace na Either)
data Result a = Error [String] | OK a
    deriving (Show)

instance Functor Result where
    fmap _ (Error e) = Error e
    fmap f (OK x) = OK (f x)

instance Applicative Result where
    pure = OK
    OK f <*> OK x = OK (f x)
    Error e <*> OK _ = Error e
    OK _ <*> Error e = Error e
    Error ef <*> Error ex = Error (ef ++ ex)

-- > OK (*) <*> OK 10 <*> OK 20
-- OK 200
-- > OK (*) <*> Error ["division by zero"] <*> OK 20
-- Error ["division by zero"]
-- > Error ["Function does not exist"] <*> Error ["Divided by zero"]
-- Error ["Function does not exist", "Divided by zero"]

-- A co seznamy? I ty jsou aplikativní. U těch operace <*> dělá to, že vezme každou funkci, tu provede na každý prvek druhého seznamu a všechny takto vzniklé seznamy spojí dohromady. Funkce pure je jednodušší, ta prostě vrátí jednoprvkový seznam.

-- TODO: Naprogramujte <*> na seznamech. (můžete použít list comprehension)
-- > [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
-- > (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- TODO: Napadlo by vás, jak by šla alternativně zadefinovat applicative u seznamů? Rada: opravdu takové seznamy existují a jmenují se ZipList.

-- TODO: Pomocí vlastností aplikativního funktoru naprogramujte funkci vracející kartézský součin dvou seznamů.

-- TODO: Navrhněte jak by mohla vypadat instance třídy Applicative pro binární strom:

-- Jako příklad funktoru, který jistě nemůže být aplikativní, můžeme uvážit typ uspořádaných dvojic, kde fmap mění druhou složku. Zde narazíme na to, že se nám nepodaří napsat pure, museli bychom totiž vytvořit z "ničeho" prvek typu a.

-- data Pair a b = Pair a b

-- instance Functor (Pair a) where
--     fmap f (Pair a b) = Pair a (f b)
-- instance Applicative (Pair a) where
--     pure y = Pair (???) y

-- Monad typeclass
-- Monády = popisují výpočty, které lze skládat dohromady

-- motivace:
-- jednoducha verze, stejna jako lookup z Data.List
my_lookup :: Eq k => k -> [(k, v)] -> Maybe v
my_lookup _ [] = Nothing
my_lookup k ((l, v):r)
  | k == l = Just v
  | otherwise = my_lookup k r
-- > my_lookup 2 [(1, "ahoj"), (3, "test"), (2, "tady")]
-- Just "tady"

--4nasobny lookup
--(prvni nalezenou hodnotu pouzije jako klic pro druhe hledani, atd.)
-- *Main> my_lookup4 1 [(1,2), (3,4), (2,3), (4,999)]
-- Just 999
my_lookup4 :: Eq a => a -> [(a, a)] -> Maybe a
my_lookup4 key1 list =
  case my_lookup key1 list of
    Nothing -> Nothing
    Just key2 ->
      case my_lookup key2 list of
        Nothing -> Nothing
        Just key3 ->
          case my_lookup key3 list of
            Nothing -> Nothing
            Just key4 -> my_lookup key4 list

--pomucka
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing f = Nothing
andThen (Just a) f = f a

--trochu lepsi my_lookup4
my_lookup4' k list =
    my_lookup k list `andThen` \k ->
    my_lookup k list `andThen` \k ->
    my_lookup k list `andThen` \k ->
    my_lookup k list

-- intuitivně podle typu funkce
    -- funktor nám umožnil vzít zabalenou hodnotu a aplikovat nějakou funkci uvnitř
    -- aplikativní funktor nám umožnil vzít zabalenou funkci a zabalenou hodnotu a aplikovat tuhle funkci na hodnotu
    -- monáda nám umožňuje vzít zabalenou hodnotu a aplikovat na ní funkci, tahle funkce vrací novou monádu

-- class Functor f where
--     (<$>) :: (a -> b) -> f a -> f b

-- class (Functor f) => Applicative f where  
--     pure :: a -> f a  
--     (<*>) :: f (a -> b) -> f a -> f b  

-- class Applicative m => Monad m where
--     return = pure
--     (>>=) :: m a -> (a -> m b) -> m b    -- tomu se říká bind

-- instance Monad Maybe where
--     Nothing >>= f = Nothing
--     Just val >>= f = f val

half x | even x    = Just (x `div` 2)
       | otherwise = Nothing

-- > (Just 4) >>= half
-- half neočekává Maybe na vstupu, nemusíme předávat wrapped value
-- díky tomu jde řetězit pomocí bind
-- > (Just 4) >>= half >>= half >>= half
-- Nothing

lookUp4M' k s =
  my_lookup k s >>= \x ->
  my_lookup x s >>= \x ->
  my_lookup x s >>= \x ->
  my_lookup x s

--lookup4 napsany pomoci Maybe monady a do-notace
--(funguje uplne stejne jako verze s andThen)
lookUp4M k s = do
  x <- my_lookup k s
  x <- my_lookup x s
  x <- my_lookup x s
  my_lookup x s

-- IO = monáda co komunikuje s okolním světem

-- funkce která převede vstup na kapitálky
zakric :: IO ()
zakric = do 
        putStrLn "Zadej text: "
        text <- getContents -- Pokud byste chtěli načíst jen jednu řádku, můžete použít getLine. Jeden znak se načte pomocí getChar.
        let output = allUpper text
        putStrLn output

allUpper :: [Char] -> [Char]        
allUpper = map (toUpper)

-- funkce která sečte čísla v souboru
sumfile :: String -> IO ()
sumfile filename = do
    vstup <- readFile filename
    let vystup = zpracujSoucet vstup    
    putStrLn vystup

zpracujSoucet :: String -> String    
zpracujSoucet v = show $ sum $ map (\x -> (read x)::Integer) $ lines v

-- hra na hádání čísel
hadej :: Integer -> Integer -> IO ()
hadej l u
  | l >= u = putStrLn "Nepodvadej"
  | l + 1 == u = putStrLn $ "Je to " ++ show l
  | otherwise = do
    let m = (l + u) `div` 2
    putStrLn $ "Hadam: " ++ show m
    res <- getLine
    case res of
      "<" -> hadej l m
      ">" -> hadej (m+1) u
      "=" -> putStrLn "vyhra"
      _ -> putStrLn "Spatny vstup hlupaku!" >> hadej l u

-- nekonecna hra
hra :: IO ()
hra = do
    hadej 0 100
    hra

-- TODO: naprogramujte program, který vezme číslo, udělá s ním nějaký výpočet a vypíše výsledek

-- TODO: naprogramujte program, který vezme číslo na vstupu a vypíše postupně průběh výpočtu collatzova problému
-- Consider the following operation on an arbitrary positive integer:
    -- If the number is even, divide it by two.
    -- If the number is odd, triple it and add one.
-- Now form a sequence by performing this operation repeatedly, beginning with any positive integer, and taking the result at each step as the input at the next.
-- The Collatz conjecture is: This process will eventually reach the number 1, regardless of which positive integer is chosen initially.


-- TODO: Naprogramujte opačnou hru, kde si počítač myslí číslo a vy se ho snažíte uhodnout
-- použijte následující boilerplate pro generování náhodného čísla:
-- main = do  
--     gen <- getStdGen  
--     askForNumber gen  
-- askForNumber :: StdGen -> IO ()  
-- askForNumber gen = do  
--     let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
--     putStr "Which number in the range from 1 to 10 am I thinking of? "  
--     ... tady už doplňte ...

-- LIST COMPREHENSIONS jsou monády
--pythagorejske trojice
trojiceNormalne =
  [ (a, b, c) | a <- [1 ..],
                b <- [1 .. a],
                c <- [1 .. b],
                a * a == b * b + c * c ]

--pomucka (interne pouzivana list-comprehensionovou syntaxi)
guard True = [()]
guard False = []

--pythagorejske trojice monadicky (interne to je uplne to same)
trojice = do
  a <- [1 ..]
  b <- [1 .. a]
  c <- [1 .. b]
  guard $ a * a == b * b + c * c
  return (a, b, c)
