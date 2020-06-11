flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:vs) = flatten vs
flatten ((x:xs):vs) = x:flatten (xs:vs)

dezip :: [(a, b)] -> ([a], [b])
dezip [] = ([], [])
dezip xs = (map fst xs, map snd xs)

rev :: [a] -> [a]
rev = foldl (flip (:)) []