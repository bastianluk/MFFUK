import Control.Monad hiding (guard)

-- Monády
-- definice
-- class Monad m where
--   (>>=)  :: m a -> (  a -> m b) -> m b
--   (>>)   :: m a ->  m b         -> m b
--   m >> k = m >>= (\_ -> k)
--   return ::   a                 -> m a
--   fail   :: String -> m a

-- příklad s monádou Maybe
-- > Just 10 >>= (\x -> Just (2*x))
-- > Just 10 >>= (\x -> Just (2*x) >>= \y -> Just (x*y+10))
-- > Just 10 >> return 5 >>= \y -> Just (y+10)

-- příklad s monádou IO, do-notace
pozdrav1 :: IO ()
pozdrav1 = putStrLn "Hello, what is your name?"
        >> getLine
        >>= \name -> putStrLn ("Hello, " ++ name ++ "!")

pozdrav2 :: IO ()
pozdrav2 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            -- main
    putStrLn "Bye!"
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  

-- IO funkce
-- putStrLn :: String -> IO ()
-- putStr :: String -> IO ()
-- putChar :: Char -> IO ()
-- print :: Show a => a -> IO ()
-- getLine :: IO String
-- getChar :: IO Char

-- funkce nad monádami

-- when :: Applicative f => Bool -> f () -> f ()
    -- if something then do some I/O action else return ()

-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
    -- sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other. The result contained in that I/O action will be a list of the results of all the I/O actions that were performed
    -- sequence [] = return []
    -- sequence (ma:mas) = do
    --   a  <- ma
    --   as <- sequence mas
    --   return (a:as)

-- TODO: Funkce, která přijme tři řádky vstupu (pomocí IO akce getLine), převede je na čísla a zkontroluje pro ně trojúhelníkovou nerovnost
--       pokud je čísla splňují, vypíše "Triangle!"

-- TODO: pomocí sequence zkontrolujte, jestli v seznamu typu [Maybe a] jsou všechny hodnoty definované (Just).

-- TODO: Napište program, který spočítá počet mezer na vstupu. Může se hodit modul Data.Char, z něj funkce isSpace.

tri :: IO ()
tri = do
    threeNums <- sequence (replicate 3 getLine)
    let threeNums' = map read threeNums :: [Int]
    when (checkTriangle threeNums') $ do
        putStrLn "Triangle!"

checkTriangle [x, y, z] = x+y > z && y+z > x && x+z > y
checkTriangle _ = False

-- mapM, mapM_
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-- mapM takes a function and a list, maps the function over the list and then sequences it. mapM_ does the same, only it throws away the result later

-- TODO: jaký je výstup tohoto výrazu a jaká je jeho hodnota?
-- > sequence (map putStrLn ["ahoj", "vice", "radku"])
-- > mapM_ print [1,2,3]  

-- forM - jako mapM, ale s přehozenými argumenty
colorsQuestion = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  


-- forever :: Applicative f => f a -> f b
-- forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever

-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- monadické skládání funkcí, - vezme dvě akce a složí je.
half x = if even x then Just (x `div` 2) else Nothing
quarter_bind x = x >>= half >>= half
quarter_composed = half >=> half

-- Monáda: selhání výpočtu s Either
-- data Either a b = Left a | Right b

-- instance Monad (Either a) where
--     return = Right
--     (Left a) >>= _ = Left a
--     (Right b) >>= f = f b

eitherHalf :: Integral a => a -> Either String a
eitherHalf x = if even x then Right (x `div` 2) else Left "cislo bylo liche"
-- > return 10 >>= eitherHalf >>= eitherHalf >>= eitherHalf

-- Monáda: seznamy + bonus jak fungují list comprehensions

-- instance Monad [] where  
--     return x = [x]  
--     xs >>= f = concat (map f xs)  
--     fail _ = []  

-- příklady:
-- > [3,4,5] >>= \x -> [x,-x]
-- > [] >>= \x -> ["bad","mad","rad"]
-- > [1,2,3] >>= \x -> []

-- > [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
-- ekvivalentní:
-- listOfTuples :: [(Int,Char)]  
-- listOfTuples = do  
--     n <- [1,2]  
--     ch <- ['a','b']  
--     return (n,ch)  

-- interpretace >>= je nedeterministická sekvence výpočtů
-- podmnožiny seznamu:
subsets [] = [[]]
subsets (l:ls) = do
    rest <- subsets ls -- nedeterministická volba prvku
    [rest, (l:rest)]
    -- vracíme více výsledků

permutations [] = [[]]
permutations l = do
    -- permutations(l, (elm:perm)) :-
    index <- take (length l) [0..]
    let elm = l !! index
    let subl = (take index l) ++ (drop (index + 1) l)
    -- select(l, elm, subl),
    perm <- permutations subl
    -- permutations(subl, perm).
    return (elm:perm)

-- TODO: Naprogramujte funkci combinationsRepeat :: Integer -> [a] -> [[a]], která vrací všechny kombinace, přičemž prvky se mohou opakovat.

-- TODO: Naprogramujte funkci combinations :: Integer -> [a] -> [[a]], která pro k vrátí všechny k-tice prvků v původním pořadí bez opakování.

--pythagorejske trojice
trojiceNormalne =
  [ (a, b, c) | a <- [1 ..],
                b <- [1 .. a],
                c <- [1 .. b],
                a * a == b * b + c * c ]

--pomucka (interne pouzivana list-comprehensionovou syntaxi)
guard True = [()]
guard False = []

-- > [1,2,3] >>= \x -> guard (even x)
-- [(),()]
-- guard "vyfiltruje" ty prvky, pro které neplatí podmínka

-- > [1,2,3] >>= \x -> guard (even x) >> return x
-- [1,3]

-- > guard True >> [7]
-- [7]

-- > guard False >> [3]
-- []

--pythagorejske trojice monadicky (interne to je uplne to same)
trojice = do
  a <- [1 ..]
  b <- [1 .. a]
  c <- [1 .. b]
  guard $ a * a == b * b + c * c
  return (a, b, c)


-- Monáda: předávání stavů


-- příklad: práce se zásobníkem
type Stack = [Int]  
  
pop' :: Stack -> (Int, Stack)  
pop' (x:xs) = (x,xs)  
  
push' :: Int -> Stack -> ((), Stack)  
push' a xs = ((),a:xs)

stackManip' :: Stack -> (Int, Stack)  
stackManip' stack = let  
    ((),newStack1) = push' 3 stack  
    (a ,newStack2) = pop' newStack1  
    in pop' newStack2

-- > stackManip' [1,2,3]
-- (1,[2,3])

-- chtěli bychom něco jako:
stackManip = do  
    push 3  
    a <- pop  
    pop
-- > runState stackManip [5,8,2,1]
-- (5,[8,2,1])


-- příklad: pseudonáhodný generátor čísel
type RandSeed = Int

randInt' :: Int -> RandSeed -> (Int, RandSeed)
randInt' max seed = (n, newseed)
    where
        newseed = (1664525 * seed + 1013904223) `mod` (2^32)
        n = (newseed `mod` max)
-- > randInt' 100 1
-- (48,1015568748)
-- > randInt' 100 1015568748
-- (67,1586005467)
-- > randInt' 100 1586005467
-- (38,2165703038)
-- > randInt' 100 2165703038
-- (65,3027450565)

rand3Int' :: Int -> RandSeed -> ([Int], RandSeed)
rand3Int' max seed0 = ([n1,n2,n3],seed3)
    where
        (n1,seed1) = randInt' max seed0
        (n2,seed2) = randInt' max seed1
        (n3,seed3) = randInt' max seed2
-- > rand3Int' 10 42
-- ([3,8,7],2479403867)

-- musíme si předávat seed :-(
-- chtěli bychom něco jako:
rand3Int max = do
    n1 <- randInt max
    n2 <- randInt max
    n3 <- randInt max
    return [n1, n2, n3]
-- > runState (rand3Int 10) 102
-- ([3,8,1],2833722951)


-- => STAVOVÁ MONÁDA

newtype State s a = State { runState :: s -> (a, s) }
-- A `State s a` is a stateful computation that manipulates a state of type s and has a result of type a.
-- Slovo newtype je v podstatě to samé, co data pouze umožňující právě jeden konstruktor.
    -- data State s a = State (s -> (a, s))
    -- runState :: State s a -> s -> (a, s)
    -- runState (State x) = x

-- fmap  :: (a -> b) -> State s a -> State s b
-- pure  :: a -> State s a
-- (<*>) :: State s (a -> b) -> State s a -> State s b
-- (>>=) :: State s a -> (a -> State s b) -> State s b

-- -- Rozbalíme typ stavu
-- fmap  :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
-- pure  :: a -> (s -> (a, s))
-- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
-- (>>=) :: (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))

instance Functor (State s) where
    fmap f (State sx) = State $ \s_0 -> -- Bereme stav 0
        let (x, s_1) = sx s_0            -- Spočítáme x a stav 1
        in  (f x, s_1)                  -- Spočítáme (f x) a stav 1

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)    -- Bere stav, vracíme x a stav

    (State sf) <*> (State sx) = State $ \s_0 ->       -- Bereme stav 0
        let (f, s_1) = sf s_0 -- Spočítáme f a stav 1
            (x, s_2) = sx s_1 -- Spočítáme x a stav 2
        in  (f x, s_2)                -- Spočítáme (f x) a stav 2

instance Monad (State s) where  
    return x = State $ \s -> (x,s) -- stejné jako pure
    (State sx) >>= f = State $ \s_0 ->
        let (x, s_1) = sx s_0 -- z sx si vytáhneme hodnotu a nový stav
            (State sg) = f x -- aplikujeme f na x, dostaneme nový State g
        in  sg s_1


pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)  

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  
-- > runState stackStuff [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])

-- TODO: napište randInt tak, aby fungoval náš kýžený do-block. Můžete použít randInt'
randInt :: Int -> State RandSeed Int
randInt max = undefined

-- > runState (randInt 20) 42
-- (13,1083814273)

rand3IntState :: Int -> State RandSeed [Int]
rand3IntState max = do
    n1 <- randInt max
    n2 <- randInt max
    n3 <- randInt max
    return [n1, n2, n3]
-- > runState (rand3IntState 10) 102
-- ([3,8,1],2833722951)

-- randIntS :: Int -> RandGen Int
-- randIntS = (state . randInt)
