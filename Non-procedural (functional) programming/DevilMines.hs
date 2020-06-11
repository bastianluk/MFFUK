-- Minesweeper with a Devil

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Char
import Data.List
-- for random seed purposes
import System.IO.Unsafe

-- State of cell
data State = Hidden | Shown
  deriving( Eq )
instance Show State where
  show Hidden = "."
  show Shown = "Impossible"

-- if a mine is there >> -1 or else adjecent mines count
type MinesCount = Int

-- for cross-checking
type Position = (Int, Int)

-- FieldCell
data FieldCell = 
  FieldCell State
            MinesCount
            Position
instance Show FieldCell where
  show (FieldCell state count _) = case state of
    Shown -> pad (show count)
    Hidden -> pad (show state)
instance Eq FieldCell where
  (FieldCell _ _ position1)==(FieldCell _ _ position2) = position1==position2


type BombCount = Int
type Height = Int
type Width = Int

-- Field of Rows of FieldCells and number of bombs - rows not represented as separate type
data Field = 
  Field [[FieldCell]]
        BombCount
        Height
        Width
instance Show Field where
    show (Field [] _ _ _) = ""
    show (Field (row:rows) bombCount height width) = getHeader width ++ showInner (Field (row:rows) bombCount height width) 0
showInner :: Field -> Int -> String
showInner (Field ([]) _ _ _) _ = ""
showInner (Field (row:rows) bombCount height width) count = pad (show count ++ ":") ++ showRow row ++ showInner (Field rows bombCount height width) (count+1) --bombCount height width are not valid here
showRow :: [FieldCell] -> String
showRow ([]) = "\n"
showRow (field:fields) = show field ++ " " ++ showRow fields

----
-- # Helpers
----

-- For Field printing purposes
getHeader :: Int -> String
getHeader length = pad "*" ++ (printHeading (take length heading)) ++ "\n"
printHeading :: [String] -> String
printHeading ([]) = ""
printHeading (n:ns) = n ++ printHeading ns
heading :: [String]
heading = [ addPadding 4 [char] | char <- ['A' .. 'Z'] ]


-- Mapping on Field
mapField :: Field -> (FieldCell -> FieldCell) -> Field
mapField (Field field bombCount height width) f = (Field (mapRows field f) bombCount height width)
mapRows :: [[FieldCell]] -> (FieldCell -> FieldCell) -> [[FieldCell]]
mapRows ([]) _ = []
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

-- Filters
filterHidden :: [FieldCell] -> [FieldCell]
filterHidden = filter isNotShown
  where
    isNotShown (FieldCell Hidden _ _) = True
    isNotShown _ = False
--
filterZeroNeighbours :: [FieldCell] -> [FieldCell]
filterZeroNeighbours = filter isZero
  where
    isZero (FieldCell _ (0) _) = True
    isZero _ = False
--
filterBombs :: [FieldCell] -> [FieldCell]
filterBombs = filter isBomb
  where
    isBomb (FieldCell _ (-1) _) = True
    isBomb _ = False


----
-- # Game
----

--Main
playDevilMines :: IO ()
playDevilMines = playGame 1 (getGameReady (mkStdGen getSeed))

playGame :: Int -> Field -> IO ()
playGame turn (Field field bombCount height width) = do {
    showField turn (Field field bombCount height width);
    putStr "Input position to uncover: \n" ;
    rowCh <- getChar ;
    let { row = ord rowCh - ord '0' } ;
    colCh <- getChar ;
    let { col = ord colCh - ord 'a' } ;
    --doNothing with the '\n' at the end of the buffer
    blank <- getChar ;
    putStr "\n" ;
    let { cell = getCell (Field field bombCount height width) (row, col)} ;
    --prevent invalid input
    if (not((row < height) && (0 <= row) && (col < width) && (0 <= col)))
      then do {
        playGame turn (Field field bombCount height width)
      }
      else if (devil turn cell (Field field bombCount height width))
      then do { putStr "YOU LOST!\n" ; return () }
      else do {
        let { newField = updateField (Field field bombCount height width) cell} ;
        nextTurn (turn+1) newField
      }   
}

-- nextTurn decides based on a check if it is a win or one more turn should be played.
nextTurn :: Int -> Field -> IO ()
nextTurn turn field = if (checkWin field)
  then do { putStr "YOU WON!\n" ; return () }
  else playGame turn field

-- If only bombs are not revealed > win
checkWin :: Field -> Bool
checkWin (Field field bombCount _ _) = (bombCount == notShownCount)
  where
    flattenedField = concat field
    notShownCount = length (filterHidden flattenedField)


-- Prints the info for a turn and the current field.
showField :: Int -> Field -> IO ()
showField turn field = putStr ("Turn: " ++ pad (show turn) ++ "\n" ++ show field)

-- Bomb checking logic
devil :: Int -> FieldCell -> Field -> Bool
devil turn (FieldCell state count position) field = if (count==(-1))
  then True
  -- After a set amount of turns - devil comes out.
  else if (turn > 10)
    then devilTurn (FieldCell state count position) field
    else False

-- Devils logic - will be backtracking based - from Field, initial seed of cell, try to find a cover for board that is possible
devilTurn :: FieldCell -> Field -> Bool
devilTurn cell (Field field bombCount height width) = backtracking cell (copyRevealed (Field field bombCount height width)) bombCount

backtracking :: FieldCell -> Field -> Int -> Bool
backtracking _ _ _ = False
--backtracking needs to assign bombs to unrevealed neighbours of revelead places such that it satisfies their values

copyRevealed :: Field -> Field
copyRevealed field = newField
  where
  newField = mapField field update
  update = keepOnlyRevealed Shown

keepOnlyRevealed :: State -> FieldCell -> FieldCell
keepOnlyRevealed desiredState (FieldCell state count position) = (FieldCell state newCount position)
  where 
    newCount = if desiredState==Shown
                 then count
                 else 0

-- Updates all the nodes that are supposed to be revealed.
updateField :: Field -> FieldCell -> Field
updateField field (FieldCell state count position) = if count==0
  then updateToShown toUpdate field
  else updateToShown [cell] field
  where
    cell = (FieldCell state count position)
    toUpdate = findAllNeighbours field [] cell

-- Very basic way to get all nodes to be revealed.
-- spreads from initial node to all other nodes that are adjecent
findAllNeighbours :: Field -> [FieldCell] -> FieldCell -> [FieldCell]
findAllNeighbours field visited (FieldCell state count position) = nub (current++concat (map findingF (toVisit)))
  where
    nhbourPositions = neighbours field position
    f = getCell field
    nhbours = map f nhbourPositions
    zeroNhbours = filterZeroNeighbours nhbours
    cell = (FieldCell state count position)
    current = (cell:nhbours)
    toVisit = zeroNhbours \\ visited
    newVisited = visited++current
    findingF = findAllNeighbours field newVisited

-- Updates all cells in a list to Shown
updateToShown :: [FieldCell] -> Field -> Field
updateToShown [] field = field
updateToShown ((FieldCell _ _ position):cells) field = updateToShown cells newField
  where
    newField = mapField field update
    update = updateCellState position
updateCellState :: Position -> FieldCell -> FieldCell
updateCellState desiredPosition (FieldCell state count position) = (FieldCell newState count position)
  where 
    newState = if desiredPosition==position
                 then Shown
                 else state



-- Gets positions of neighbours of in a field based on initial position.
neighbours :: Field -> Position -> [Position]
neighbours (Field _ _ height width) (row, col) = filter inGrid [ (row-1,col-1), (row-1,col), (row-1,col+1), (row,col-1), (row,col), (row,col+1), (row+1,col-1), (row+1,col), (row+1,col+1) ]
  where
    inGrid (s,t) = 0 <= s && s < height && 0<=t && t < width

-- Gets a cell from a field based on its position
getCell :: Field -> Position -> FieldCell
getCell (Field field _ _ _) (row, col) = field!!row!!col

----
-- ## Preparation
----

-- Prepare precalculated and game ready (mine) field.
getGameReady :: StdGen -> Field
getGameReady gen = mapField field updateBombs
  where
    field = getNewField 10 8 8 gen
    updateBombs = countBombNeighbours field

countBombNeighbours :: Field -> FieldCell -> FieldCell
countBombNeighbours field (FieldCell state count position) = (FieldCell state newCount position)
  where 
    f = getCell field
    newCount = if count==(-1)
                 then count
                 else length (filterBombs (map f (neighbours field position)))

-- Gets a field with bombs - the adjecency values need to be calculated.
getNewField :: Int -> Int -> Int -> StdGen -> Field
getNewField mineCount height width gen = (Field [ getRow bombIndexes index width | index <- [0..height-1], let bombIndexes = getRandomBombIndexes mineCount 0 ((width*height)-1) gen] mineCount height width)

--
getRandomBombIndexes :: Int -> Int -> Int -> StdGen -> [Int]
getRandomBombIndexes count from to gen= take count (shuffle [from .. to] gen)

-- For now just "per build" seed getter
getSeed :: Int
getSeed = unsafePerformIO (getStdRandom (randomR (0, 322)))

-- Looked for a generic way of getting random indexes for bombs - shuffle all possible positions and then in the usage I take just a set amount based on the difficuty wanted.
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
