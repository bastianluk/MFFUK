-- REKURZE A HIGHER-ORDER FUNKCE

-- TODO: my_takeWhile f xs -- funkce která vrací ze vstupního xs prvky dokud platí f pro aktuální prvek
-- > my_takeWhile (<5) [1,3,4,5,8,7,3,1]
-- [1,3,4]
-- > my_takeWhile odd [1,3,5,4,5,3,1]
-- [1,3,5]

-- my_filter f [] = []
-- my_filter f (x:xs)
--     | f x = x:(my_filter f xs)
--     | otherwise = (my_filter f xs)

my_takeWhile :: (a->Bool) -> [a] -> [a]
my_takeWhile f [] = []
my_takeWhile f (x:xs)
    | f x = x:(my_takeWhile f xs)
    | otherwise = []

-- > takeWhile odd [1,1,2,3]

-- poznámka: dropWhile funguje podobně, jen vrátí zbytek seznamu
-- > dropWhile odd [1,3,5,4,5,3,1]
-- [4,5,3,1]

-- TODO: my_span f xs = (ys, zs) -- rozdělí xs na dvojici seznamů, ys v dvojici obsahuje prvky, 
-- pro které podmínka f platila, zs obsahuje prvky od bodu, kdy f přestala platit (použijte takeWhile a dropWhile)
-- > my_span odd [1,3,5,4,5,3,1]
-- ([1,3,5],[4,5,3,1])
my_span f xs = (takeWhile f xs, dropWhile f xs)

-- plus :: 
-- plusJedna = (plus 1) :: Int -> Int
-- plus x y = x + y

-- TODO: group xs -- v seznamu xs najde sousední hodnoty, které jsou shodné a vloží je do zvláštních seznamů, použijte span
-- > group [1,1,1,2,3,3]
-- [[1,1,1],[2],[3,3]]



my_group :: Eq a => [a] -> [[a]]
my_group [] = []
my_group (x:xs) = (x:front):(my_group back)
    where (front, back) = my_span (==x) xs

-- TODO navíc: velmi jednoduše (pomocí map) získejte z výstupu group délky nalezených bloků stejných hodnot
-- výstup pro předchozí vstup: [3,1,2]

-- TODO (navíc): my_words str -- rozdělí string na seznam podstringů, které v původním str byly oddělené mezerami
-- my_words "ahoj světe" = ["ahoj", "světe"]
-- my_words :: 

-- Porovnejte následující definice foldl a foldr:

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs) 

-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
-- foldl f z []     = z                  
-- foldl f z (x:xs) = foldl f (f z x) xs

-- my_ [] = INIT
-- my_ (x:xs) = x OP (my_sum xs)
my_foldr f z []     = z 
my_foldr f z (x:xs) = f x (my_foldr f z xs)

-- TODO: my_sum xs -- součet prvků seznamu xs pomocí foldl
my_sum xs = foldr (+) 0 xs

-- TODO: scalarMult xs -- skalární součin dvou seznamů pomocí my_sum a zipWith

-- scalarMult xs ys = my_sum (zipWith (*) xs ys)

-- Pomocí foldl a foldr naprogramujte rev xs, který otočí vstupní seznam xs
-- jako první argument fold použijte lambda funkci, uvnitř pak buď zřetězení seznamů (++) nebo (:)
-- TODO: implementace rev_l pomocí foldl:

-- TODO: implementace rev_r pomocí foldr:

-- TODO: diffs xs -- seznam čísel převede na (o jedna kratší) seznam rozdílů sousedních prvků
--       zkuste ale NEpoužít rekurzi (zamyslete se, jak by šlo použít zipWith, bude se také hodit funkce tail)
-- > diffs [1,2,3,4,6]
-- [1,1,1,2]

diffs :: Num a => [a] -> [a]
diffs xs = zipWith (-) (tail xs) xs

-- Obecně se v Haskellu snažíme spíš nepoužívat rekurzi přímo, ale skrz rekurzivní schémata
-- pomocí funkcí vyššího řádu: map, filter, fold*, zip, zipWith, take, drop, takeWhile, dropWhile, ...

-- RANGES + LIST COMPREHENSIONS

-- concat xss = [x|xs<-xss, x<-xs]

-- TODO: násobky devíti od čísla 90 pozpátku k nule (v Haskellu zapsáno pomocí jednoho výrazu o 10 znacích)

-- TODO: mensiNez n xs -- vrátí seznam xs, který obsahuje prvky menší než n (místo filter použijte list comprehension)

-- TODO: removeNonUppercase str -- vrátí string str bez znaků, které nejsou uppercase (pro seznam velkých písmen použijte ['A'..'Z'])

-- TODO: pyth n -- vrátí seznam Pythagorejských trojic se stranou menší nebo rovno n (bez opakování)
-- > pyth 20
-- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]

-- NEKONEČNÉ SEZNAMY

-- TODO: seznam obsahující všechna lichá čísla, zapsáno pomocí range (můžete zkontrolovat pomocí take)
-- TODO: seznam obsahující všechna lichá čísla, zapsáno pomocí rekurzivní definice

-- poznámka: existují funkce repeat a cycle, které vytvoří 
--           nekonečný seznam pomocí opakování jednoho čísla (repeat) a nebo seznamu (cycle)
--           pokud si nejste jisti, jak byste je naimplementovali, zkuste to (na základě předchozího TODO)

-- TODO: fibonacciho čísla pomocí rekurzivní definice
-- TODO: fibonacciho čísla pomocí funkcí zipWith + tail
-- TODO: seznam všech prvočísel, pro testování prvočíselnosti čísla v tomto seznamu se použije seznam všech prvočísel :-)

-- seznamVsechPrvocisel = [2,3,5,7]
seznamVsechPrvocisel = 2:(filter jePrvo [3..])
    where
        jePrvo x = and (map (nedelitelne x) (mensiPrvocisla x))
        nedelitelne x prvo = (x `mod` prvo) /= 0
        mensiPrvocisla x = takeWhile (\p -> p*p <= x) seznamVsechPrvocisel

-- TODO navíc (netýká se nekonečných seznamů): quicksort, insertsort, mergesort

-- VLASTNÍ TYPY

-- TODO: Vlastní typ Complex a implementace násobení komplexních čísel

-- funkce lookup key list, která najde hodnotu na základě klíče v seznamu dvojic (klíč, hodnota)
-- > lookup "Martina" [("Marie", 100), ("Marek", 392), ("Martina", 1929)] 
-- Just 1929
-- > lookup "Honza" [("Marie", 100), ("Marek", 392), ("Martina", 1929)] 
-- Nothing

-- TODO: Vlastní typ Mozna, který bude fungovat stejně jako Maybe