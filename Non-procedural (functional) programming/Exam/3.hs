-- Excercise 3 - Bastian Lukas


-- a)
-- Binarni Vyhledavaci Strom je bud:
-- End ~ konec ~ ekvivalent null
-- Node Left Value Right ~ kdy jde o nejaky vrchol ve stromu, ktery ma levy podstrom, svou hodnotu a pravy podstrom. Podrstromy opet typu BVS
data BVS a = End | Node (BVS a) a (BVS a)
 deriving (Show)

-- b) && c)
-- pruhy (Node (Node (Node End 1 End) 2 (Node End 4 End)) 5 (Node End 6 End)) [3, 4, 6]
pruhy :: BVS Int -> [Int] -> [BVS Int]
pruhy bvs seznam = internalpruhy bvs (0:seznam)

internalpruhy :: Ord a => BVS a -> [a] -> [BVS a]
internalpruhy bvs (a:[]) = [getBVSLast bvs a]
internalpruhy bvs (a:b:rest) = ((getBVS bvs a b):internalpruhy bvs (b:rest))

getBVS :: Ord a => BVS a -> a -> a -> BVS a
getBVS (End) _ _ = (End)
getBVS (Node l v r) a b
    | a <= v && v < b = (Node leftSub v rightSub)
    | otherwise = newBest
  where
    leftSub = getBVS l a b
    rightSub = getBVS r a b
    newBest = getBest leftSub rightSub

-- za zadani by se asi dala nejak odvodit horni hranice a pouzit getBVS nebo proste passovat predicate, podle ktereho se to vyhodnocuje
getBVSLast :: Ord a => BVS a -> a -> BVS a
getBVSLast (End) _ = (End)
getBVSLast (Node l v r) a
    | a <= v = (Node leftSub v rightSub)
    | otherwise = newBest
  where
    leftSub = getBVSLast l a
    rightSub = getBVSLast r a
    newBest = getBest leftSub rightSub

-- spojovani dvou vetvy vypoctu
getBest :: BVS a -> BVS a -> BVS a
getBest (End) (End) = (End)
getBest (End) r = r
getBest l (End) = l
getBest l r = getLeftMostInRight l r

-- zpusob napojeni dvou podstromu, pokud jsou v nich validni hodnoty - i nejlevejsi hodnota praveho podstromu by mela byt vetsi nez jakakoliv v levem => staci ji najit a jako levy podstrom ji dat cely puvodni levy
getLeftMostInRight :: BVS a -> BVS a -> BVS a
getLeftMostInRight l (Node (End) v r) = (Node l v r)
getLeftMostInRight l (Node rl v r) = (Node (getLeftMostInRight l rl) v r)

-- d) korektnost
-- postupne beru okenka/intervaly, pricemz ja jsem pro zacatek pridal okenko 0 az prvni uzivatelem zadane cislo
-- pokud se v prubehu vypoctu stane, ze pro nejaky (Node l v r) pro v podminka neplati, ale pro neco z l nebo r ano, tak se vytvori strom podle logiky popsane vyse u getLeftMostInRight
-- samotne vyhodnoceni je o kontrole podminky pro v ~ value, jestli spada do okenka a pokud ano, tak si ji necham a rekurzivne volam na levy a na pravy podstrom stejne vyhodnoceni
-- pokud ne, tak se kontroluji podstromy, ktere se vsam musi spojit do validniho stromu (popsano vyse)