-- Naše oblíbená definice stromu
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- Kinds (druhy)
-- Všimněte si, že typové konstruktory přijímají jiné typy jako parametry a ve výsledku vytvoří konkrétní typy.
-- například typový konstruktor `Tree a` přijímá libovolný typ `a` a vraci konkrétní typ, třeba `Tree Int`
-- To je trochu jako když funkce přijímají nějaké hodnoty aby vytvořily jinou hodnotu.
-- Každá hodnota má svůj typ, stejně tak každý typ má svůj kind (druh)
-- prohlédnout si kind nějakého typu můžete pomocí :k Int
-- ve výpisu příkazu :k se můžete setkat s hvězdičkou *, ta značí konkrétní typ, tj. Int, Bool, [Char], Tree Integer
-- pak také můžete použít :k pro typeclass, zkuste třeba `:k Eq`. Druh Constraint znamená nějaké omezení na typovou třídu.

-- TODO: Když budete předpokládat, že -> ve výpisu :k funguje podobně jako u typů funkcí, 
--       najděte příklad typového konstruktoru s druhem * -> *

-- TODO: Najděte příklad typu s druhem * -> * -> *, případně ho zadefinujte pomocí data

-- TODO: Co se stane, když typu z předchozí úlohy předám jen první ze dvou argumentů?

-- TODO: Dokážete vysvětlit význam `:k Functor` ?
-- pro přípomenutí:
-- class Functor f where  
--     fmap :: (a -> b) -> f a -> f b  


-- Typeclass Foldable
-- "Class of data structures that can be folded to a summary value."
-- Typová třída zobecňující "skládání" kontejnerů do nějaké hodnoty, známe ze seznamů:
-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs) 
-- tedy procházíme seznam zleva doprava a "nahrazujeme" dvojtečku nějakou funkcí. Konec seznamu nahradíme hodnotou z
-- foldr (+) 0 (1:(2:(3:[]))) = (1+(2+(3+0)))

-- Jak by takové procházení vypadalo pro strom?
-- chtěli bychom třeba takovou implementaci, aby:
-- > foldr (:) [] (Node 2 (Node 1 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)) = [1, 2, 3]

-- TODO: Napište instanci Foldable pro typový konstruktor Tree
-- class Foldable (t :: * -> *) where
--     foldr :: (a -> b -> b) -> b -> t a -> b


-- Procvičení typu Maybe
-- data Maybe a = Just a | Nothing
-- Nothing = něco jako null, hodnota je buď prázdná nebo nějaká konkrétní jiná

-- TODO: Naprogramujte funkci indexOf :: Eq a => a -> [a] -> Maybe Integer, která vezme prvek a vrátí index jeho prvního výskytu. Pokud se v seznamu nevyskytuje, nevrátíme nic.

-- TODO: Naprogramujte funkci maybeZip :: [a] -> [b] -> [(Maybe a, Maybe b)], která se chová jako zip pro stejně dlouhé seznamy a pro různě dlouhé seznamy pak po dojití na konec kratšího seznamu bude vracet na daném místě Nothing.

-- TODO: Naprogramujte funkci takeExactly :: Int -> [a] -> Maybe [a], která vrátí prvních k prvků seznamů pouze, pokud je seznam dost dlouhý.

-- podobný je také typ Either, který je buď typ `a` nebo typ `b`
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  
type Vysledek a = Either String a
safeDiv :: Int -> Int -> Vysledek Int
safeDiv x 0 = Left "Hele neděl nulou jo"
safeDiv x y = Right (div x y)

-- Typeclass Functor
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- typové konstruktory, které se chovají jako kontejner / kontext výpočtu.
-- různé funktory: Maybe, Tree, [], Either
-- jak se dá koukat na fmap :: (a -> b) -> f a -> f b
    -- 1) fmap přijímá funkci `a -> b` a kontejner `f` s hodnotou typu `a`. Vrací kontejner `f` s hodnotou typu `b` 
    -- 2) fmap přijímá funkci `a -> b` a vrátí novou funkci `f a -> f b`, která funguje na kontejnery typu `f`
-- musí splňovat dvě podmínky:
    -- 1) fmap id = id
    -- 2) fmap (f . g) = fmap f . fmap g
-- navíc protože fmap má jako argument funkci typu `a -> b`, musí kontejner být parametrizován a musí umět obalovat libovolné typy.
-- [Int] není funktor, protože ho nemůžeme změnit třeba funkcí `Int -> Bool` (jak by vypadala instance?). 
-- > :k Functor
-- Functor :: (* -> *) -> Constraint

-- fmap má svou infixovou variantu <$>

-- TODO: Co se stane?
--       > (*2) <$> [1, 2, 3]
--       > fmap (*2) Nothing
--       > (*2) <$> (Just 10)
--       > fmap (*2) []
--       > fmap (*2) (Left 2)
--       > fmap (*2) (Right 4)

-- TODO: Kdy může být uspořádaná dvojice funktor? Podívejte se na kind konstruktoru dvojice `:k (,)`

-- TODO: Může být abstraktní typ, který reprezentuje uspořádanou množinu s vyhledáváním, vkládáním nebo nalezením následníka, funktor?


-- Typeclass Applicative = Aplikativní funktory
-- víme co se stane, když zavoláme `fmap (*2) (Just 10)` - prostě aplikujeme funkci (*2) na 10 a výsledek zabalíme do Just.
-- co když ale zavoláme `fmap (*) (Just 10)`?

-- TODO: Spusťe a doplňte otazníky
-- ghci> let a = fmap (*) ([1,2,3,4])
-- ghci> :t a
-- ???
-- ghci> fmap ??? a
-- [2,4,6,8]

-- co kdybych ale měl zabalené hodnoty `Just (3 *)` a `Just 5`, chtěl bych vytáhnout funkci (3 *) a "namapovat" ji na `Just 5`? To už s normálním fmap nejde.

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

-- -- pure y = Pair (???) y

-- Monad typeclass
-- zatím jen intuitivně:
    -- funktor nám umožnil vzít zabalenou hodnotu a aplikovat nějakou funkci uvnitř
    -- aplikativní funktor nám umožnil vzít zabalenou funkci a zabalenou hodnotu a aplikovat tuhle funkci na hodnotu
    -- monáda nám umožňuje vzít zabalenou hodnotu a aplikovat na ní funkci, tahle funkce ale vrací monádu

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- class (Functor f) => Applicative f where  
--     pure :: a -> f a  
--     (<*>) :: f (a -> b) -> f a -> f b  

-- class Applicative m => Monad m where
--     return = pure
--     (>>=) :: m a -> (a -> m b) -> m b    -- tomu se říká bind

half x | even x    = Just (x `div` 2)
       | otherwise = Nothing

-- > (Just 4) >>= half
-- half neočekává Maybe na vstupu, nemusíme předávat wrapped value
-- díky tomu jde řetězit pomocí bind
-- > (Just 4) >>= half >>= half >>= half
-- Nothing

-- instance Monad Maybe where
--     Nothing >>= f = Nothing
--     Just val >>= f = f val