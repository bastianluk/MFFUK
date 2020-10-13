flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:vs) = flatten vs
flatten ((x:xs):vs) = x:flatten (xs:vs)

dezip :: [(a, b)] -> ([a], [b])
dezip [] = ([], [])
dezip xs = (map dezipfst xs, map dezipsnd xs)
dezipfst (a, b) = a
dezipsnd (a, b) = b


rev :: [a] -> [a]
rev = foldl (flip (:)) []