-- Excercise 4 - Bastian Lukas

-- a) rle recursion
rle :: Eq a => [a] -> [Either a (a,Int)]
rle [] = []
rle (x:xs)
    | length == 1 = ((Left x) : rle newXs)
    | otherwise = ((Right (x, length)) : rle newXs)
  where
    length = isSame x (x:xs)
    newXs = drop length (x:xs)
-- not sure I can use takewhile - this is the eqvivalent
isSame :: Eq a => a -> [a] -> Int
isSame x ([]) = 0
isSame x (y:ys)
  | x == y = 1 + isSame x ys
  | otherwise = 0

-- b) rlef - rle folding
-- nejdrive iterace pres cely array a pocitani toho, kolikrat se co objevilo za sebou
-- pote namapovat vsechno do spravne podoby ((char, 1) => Left x; (char,i), i > 1 => Right (char, i))
rlef:: Eq a => [a] -> [Either a (a,Int)]
rlef array = map countToEither (foldl count [] array)

count:: Eq a => [(a,Int)] -> a -> [(a,Int)]
count x c
    | x == [] = [(c, 1)] --start
    | otherwise = (take (len - 1) x ) ++ (incOrAdd (last x) c) -- vzit vsechno krome posledni a k poslednimu zkusit pridat dalsi znak
  where
    len = length x

incOrAdd:: Eq a => (a,Int) -> a -> [(a,Int)]
incOrAdd (c, n) newC
    | c == newC = [(c,n+1)] -- pokud je znak stejny, zvysit pocet
    | otherwise = [(c,n), (newC, 1)] -- pokud ne, tak vratit posledni jak byl a pridat novy znak s vyskytem 1

-- pro namapovani poctu vyskytu na spravnou formu
countToEither:: (a, Int) -> Either a (a,Int)
countToEither (c, n) 
  | n == 1 = Left c
  | otherwise = Right (c, n)

-- c) && d) rld - types + function
rld :: Eq a => [Either a (a,Int)] -> [a]
rld = concat . map getArrayFromEither
-- this generates the required number of occurances based on the either
getArrayFromEither :: Either a (a,Int) -> [a]
getArrayFromEither (Left x) = [x]
getArrayFromEither (Right (x, i)) = take i (repeat x)

-- e)
-- jak rle a rlef potrebuji konec seznamu pro zastaveni se vydani vysledku, ale rld mapuje a spojuje postupne co prave vypocitalo tedy i `rld (repeat (Left 'a'))` bude fungovat