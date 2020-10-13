cumsum :: Num a => [[a]] -> [[a]]
cumsum (xs:ys) = sumColumnWise ([]:(sumRowWise (xs:ys)))

sumColumnWise :: Num a => [[a]] -> [[a]]
sumColumnWise (xs:[]) = ([])
sumColumnWise (xs:ys:zs) = (result:sumColumnWise (result:zs))
 where result = addElementWise xs ys

addElementWise :: Num a => [a] -> [a] -> [a]
addElementWise [] [] = []
addElementWise [] (y:ys) = (y:ys)
addElementWise (x:xs) (y:ys) = (x+y) : addElementWise xs ys

sumRowWise :: Num a => [[a]] -> [[a]]
sumRowWise (xs:[]) = (sumRow xs:[])
sumRowWise (xs:ys) = (sumRow xs:sumRowWise ys)

sumRow :: Num a => [a] -> [a]
sumRow (x:[]) = (x:[])
sumRow (x:y:xs) = (x:sumRow (row:xs))
 where row = x + y