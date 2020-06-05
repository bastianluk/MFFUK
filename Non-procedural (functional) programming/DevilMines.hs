-- Minesweeber with a Devil

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Char
import Data.List
import Data.Time
import System.IO.Unsafe

--data HiddenState = Hidden | Bomb
--instance Show HiddenState where
--  show Hidden = "."
--  show Bomb = "Impossible"

--data ShownState = Shown | Marked
--instance Show ShownState where
--  show Shown = "Impossible"
--  show Marked = "x"

-- States of cells on a board
-- Safe, Marked and Mines1-8 represent the revealed state, Bomb is losing state and NotRevealed is default state
data State = Hidden | Shown
instance Show State where
  show Hidden = "."
  show Shown = "Impossible"

type MinesCount = Int

type Position = (Int, Int)

data FieldCell = 
  FieldCell State
            MinesCount
            Position
instance Show FieldCell where
  show (FieldCell state count _) = case state of
    Shown -> pad (show count)
    Hidden -> pad (show state)--pad (show count) --pad (show state)
instance Eq FieldCell where
  (FieldCell state1 count1 position1)==(FieldCell state2 count2 position2) = position1==position2

data Field = 
  Field [[FieldCell]]
instance Show Field where
    show (Field (row:rows)) = getHeader (length row) ++ showInner (Field (row:rows)) 0 
showInner :: Field -> Int -> String
showInner (Field (row:[])) count = pad (show count ++ ":") ++ showRow row
showInner (Field (row:rows)) count = pad (show count ++ ":") ++ showRow row ++ showInner (Field rows) (count+1)
showRow :: [FieldCell] -> String
showRow (field:[]) = show field ++ "\n"
showRow (field:fields) = show field ++ " " ++ showRow fields

getHeader :: Int -> String
getHeader length = pad "*" ++ (printHeading (take length heading)) ++ "\n"
printHeading :: [String] -> String
printHeading (n:[]) = n
printHeading (n:ns) = n ++ printHeading ns
heading :: [String]
heading = [ addPadding 4 [char] | char <- ['A' .. 'Z'] ]



----
-- # Helpers
----

-- Mapping on Field
mapField :: Field -> (FieldCell -> FieldCell) -> Field
mapField (Field field) f = (Field (mapRows field f))
mapRows :: [[FieldCell]] -> (FieldCell -> FieldCell) -> [[FieldCell]]
mapRows (row:[]) f = ((map f row):[])
mapRows (row:rows) f = ((map f row):(mapRows rows f))

-- Padding for printing
pad :: String -> String
pad s = addPadding 3 s
addPadding :: Int -> String -> String
addPadding n st
 | len <= n = st ++ replicate (n - len) ' '
 | otherwise = take n st
 where
 len = length st



----
-- # Game
----

--playDevilMines
playDevilMines = playGame 1 getGameReady
-- :: [[Int]] -> (Int,Int) -> [(Int,Int)]

playGame :: Int -> Field -> IO ()
playGame turn field = do {
    showField turn field;
    putStr "Input position to uncover: \n" ;
    rowCh <- getChar ;
    let { row = ord rowCh - ord '0' } ;
    colCh <- getChar ;
    let { col = ord colCh - ord 'a' } ;
    --doNothing with the '\n' at the end of the buffer
    blank <- getChar ;
    putStr "\n" ;
    let { cell = getCell field (row, col)} ;
    if (devil turn cell field)
      then do { putStr "YOU LOST!\n" ; return () }
      else do {
        let { newField = updateField field cell} ;
        nextTurn (turn+1) newField
      }   
}

nextTurn :: Int -> Field -> IO ()
nextTurn turn field = if (checkWin field)
  then do { putStr "YOU WON!\n" ; return () }
  else playGame turn field

checkWin :: Field -> Bool
checkWin (Field field) = (bombCount == notShownCount)
  where
    flattenedField = concat field
    bombCount = length (filterBombs flattenedField)
    notShownCount = length (filterHidden flattenedField)

filterHidden :: [FieldCell] -> [FieldCell]
filterHidden = filter isNotShown
  where
    isNotShown (FieldCell Hidden _ _) = True
    isNotShown _ = False


showField :: Int -> Field -> IO ()
showField turn field = putStr ("Turn: " ++ pad (show turn) ++ "\n" ++ show field)

devil :: Int -> FieldCell -> Field -> Bool
devil turn (FieldCell state count position) field = if (count==(-1))
  then True
  else if (turn > 10)
    then devilTurn (FieldCell state count position) field
    else False

devilTurn :: FieldCell -> Field -> Bool
devilTurn (FieldCell state count position) field = False


updateField :: Field -> FieldCell -> Field
updateField field (FieldCell state count position) = if count==0
  then updateToShown toUpdate field
  else updateToShown [cell] field
  where
    cell = (FieldCell state count position)
    toUpdate = findAllNeighbours field [] cell

findAllNeighbours :: Field -> [FieldCell] -> FieldCell -> [FieldCell]
findAllNeighbours field visited (FieldCell state count position)= nub (cell:concat (map findingF toVisit))
  where
    nhbourPositions = neighbours field position
    f = getCell field
    nhbours = map f (neighbours field position)
    zeroNhbours = filterZeroNeighbours nhbours
    cell = (FieldCell state count position)
    newVisited = visited++zeroNhbours
    toVisit = zeroNhbours \\ visited
    findingF = findAllInner field newVisited

findAllInner :: Field -> [FieldCell] -> FieldCell -> [FieldCell]
findAllInner field visited (FieldCell state count position) = nub (current++concat (map (findAllInner field newVisited) (toVisit)))
  where
    nhbourPositions = neighbours field position
    f = getCell field
    nhbours = map f (neighbours field position)
    zeroNhbours = filterZeroNeighbours nhbours
    cell = (FieldCell state count position)
    current = (cell:nhbours)
    toVisit = zeroNhbours \\ visited
    newVisited = visited++current
    findingF = findAllInner field newVisited

updateToShown :: [FieldCell] -> Field -> Field
updateToShown [] field = field
updateToShown ((FieldCell state count position):cells) field = updateToShown cells newField
  where
    newField = mapField field update
    update = updateCellState field position

updateCellState :: Field -> Position -> FieldCell -> FieldCell
updateCellState field desiredPosition (FieldCell state count position) = (FieldCell newState count position)
  where 
    newState = if desiredPosition==position
                 then Shown
                 else state

filterZeroNeighbours :: [FieldCell] -> [FieldCell]
filterZeroNeighbours = filter isZero
  where
    isZero (FieldCell _ (0) _) = True
    isZero _ = False

neighbours :: Field -> Position -> [Position]
neighbours (Field ((firstRow):rows)) (row, col) = filter inGrid [ (row-1,col-1), (row-1,col), (row-1,col+1), (row,col-1), (row,col), (row,col+1), (row+1,col-1), (row+1,col), (row+1,col+1) ]
  where
    inGrid (s,t) = 0 <= s && s <= rowCount && 0<=t && t <= colCount
    rowCount = length rows
    colCount = ((length firstRow) -1)

getCell :: Field -> Position -> FieldCell
getCell (Field field) (row, col) = field!!row!!col

----
-- ## Preparation
----

--Prepare precalculated and ready game (mine) field
getGameReady :: Field
getGameReady = mapField field updateBombs
  where
    field = getNewField 10 8 8
    updateBombs = countBombNeighbours field

countBombNeighbours :: Field -> FieldCell -> FieldCell
countBombNeighbours field (FieldCell state count position) = (FieldCell state newCount position)
  where 
    f = getCell field
    newCount = if count==(-1)
                 then count
                 else length (filterBombs (map f (neighbours field position)))

filterBombs :: [FieldCell] -> [FieldCell]
filterBombs = filter isBomb
  where
    isBomb (FieldCell _ (-1) _) = True
    isBomb _ = False

getNewField :: Int -> Int -> Int -> Field
getNewField mineCount height width = (Field [ getRow bombIndexes index width | index <- [0..height-1], let bombIndexes = getRandomBombIndexes mineCount 0 ((width*height)-1)] )

getRandomBombIndexes :: Int -> Int -> Int -> [Int]
getRandomBombIndexes count from to = take count (shuffle [from .. to] (mkStdGen sessionSeed))

sessionSeed :: Int
--sessionSeed = fromIntegral (case (unsafePerformIO getCurrentTime) of (TimeOfDay s m h) -> s)
sessionSeed = unsafePerformIO (getStdRandom (randomR (0, 322)))

--
shuffle :: [a] -> StdGen -> [a]
shuffle xs gen = runST (do
                  g <- newSTRef gen
                  let randomRST lohi = do
                        (a,s') <- liftM (randomR lohi) (readSTRef g)
                        writeSTRef g s'
                        return a
                  ar <- newArray n xs
                  xs' <- forM [1..n] $ \i -> do
                          j <- randomRST (i,n)
                          vi <- readArray ar i
                          vj <- readArray ar j
                          writeArray ar j vi
                          return vj
                  return xs')
                where
                  n = length xs
                  newArray :: Int -> [a] -> ST s (STArray s Int a)
                  newArray n xs =  newListArray (1,n) xs

getRow :: [Int] -> Int -> Int -> [FieldCell]
getRow bombIndexes rowNumber rowLength = [ getNewCell (elem (rowNumber * rowLength + index) bombIndexes) (rowNumber,index)| index <- [0..rowLength-1]]

getNewCell :: Bool -> Position -> FieldCell
getNewCell isBomb point = if isBomb
                    then (FieldCell Hidden (-1) point)
                    else (FieldCell Hidden (0) point)