obvod a b = 2 * (a + b)

obsah = (*)

soucet2D (x1, y1) (x2, y2) = (x1+x2, y1+y2)

faktorial 1 = 1
faktorial x = x * faktorial (x - 1)

delka [] = 0
delka (x:xs) = 1 + delka xs

zdvojnasobVsechny [] = []
zdvojnasobVsechny (x:xs) = (2*x):(zdvojnasobVsechny xs)

zdvonasobJeden :: Int -> Int
zdvonasobJeden x = 2*x

-- > map zdvonasobJeden [1,2,3,4]

secti :: Int -> Int -> Int
secti x y = x + y

my_map :: (a -> b) -> [a] -> [b]
my_map _ [] = []
my_map f (x:xs) = (f x):(my_map f xs)

lichy x = mod x 2 == 1

lichySeznam = map lichy

faktorial2 n
    | n < 0     = error "nedavej cisla mensi nez nula"
    | n == 0    = 1
    | otherwise = n * (faktorial2 (n-1))

-- last_element seznam = posledni prvek
-- last_element :: ??? TODO typ
last_element :: [a] -> a
last_element [] = error "Prazdny seznam nema poslední prvek!"
last_element [x] = x
-- last_element (_:xs) = -- TODO

-- TODO: predposledni seznam = predposledni prvek

-- nty n seznam = nty prvek
-- nty :: ??? TODO typ
nty _ [] = error "Příliš veliký index!"
nty 0 (x:xs) = x
-- nty n (x:xs) = -- ??? TODO

-- my_zip sez1 sez2 = []
--      zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]
--      zip "ahoj" "ah" = [('a', 'a'), ('h', 'h')]
my_zip :: [a] -> [b] -> [(a, b)]
-- my_zip ??? = [] -- TODO
-- my_zip ??? = [] -- TODO
my_zip (x:xs) (y:ys) = (x, y):(my_zip xs ys)

-- dedup sez1
-- dedup [2, 4, 4, 4, 6, 6, 8, 4]
--      [2, 4, 6, 8, 4]
dedup (x:[]) = [x]
dedup (x:(y:s)) 
 | x == y = dedup (y:s)
 | otherwise = x:(dedup (y:s))

-- zipWith f sez1 sez2
-- my_sum seznam = součet prvků
my_sum [] = 0
my_sum (x:xs) = x + my_sum xs


-- foldl f init seznam
my_product :: Num a => [a] -> a
my_product = foldl (*) 1
-- my_filter f sez
-- my_filter lichy [1, 2, 3, 4, 5]
--     []

-- TODO: qsort
