import System.Random
import System.IO.Unsafe

import Data.List -- our import

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
                 , width :: Int
                 , height :: Int
                 } deriving (Show)

rand :: Int -> Int
-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do {
                r <- fmap (flip mod $ length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)
             }

----------------------------------------------------------------------------------------------------------------------

-- Labyrinth Representation
-- initialize a Maze of the specified dimensions
makeMaze :: Int -> Int -> Maze
makeMaze w h = Maze {cells = ( take (w*h) $ repeat (True,True)), width = w, height = h}

-- Construction Algorithm
-- use the assistant-statements in a specific way, in order to produce a Perfect Maze
kruskal :: Maze -> Maze
kruskal m =
    let 
        s = sets (width m) (height m)
        w = walls (width m) (height m)
        sw = shuffled_walls (width m) (height m)
        newsw = outerLoop sw s
        newcells = createCells (width m) (height m) (cells m) 1 newsw
    in
        Maze { cells = newcells, width = width m, height = height m}

-- Assistant-Statements for Construction
-- create the cells of the Perfect Maze
createCells :: Int -> Int -> [(Bool,Bool)] -> Int -> [(Int,Int)] -> [(Bool,Bool)]
createCells _ _ [] _ _ = []
createCells w h cl i wl =
    let
        (r,d) = head cl
        walls = accessIndexWallList wl i
        wall1 = (i,i+1)
        wall2 = (i,i+w)
        bool1 = (containsWall walls wall1) || (mod i w) == 0
        bool2 = (containsWall walls wall2) || (i > w*h-w && i <= w*h)
    in
        [(bool1,bool2)] ++ createCells w h (tail cl) (i+1) wl

-- the outer loop of the Kruskal's Algorithm
outerLoop :: [(Int,Int)] -> [[(Int)]] -> [(Int,Int)]
outerLoop [] _ = []
outerLoop swl sl = 
    let 
        (ci,cj) = head swl
        seti = accessIndex sl ci
        setj = accessIndex sl cj
    in 
        if ( (contains setj ci) == False && (contains seti cj) == False )
        then
            let
                js = ourUnion seti setj
                newsl = joinedSetLoop1 js js sl
            in
                outerLoop (tail swl) newsl
        else
            [head swl] ++ outerLoop (tail swl) sl

-- the innner loop of the Kruskal's Algorithm
-- returns the renewed set's list
joinedSetLoop1 :: [(Int)] -> [(Int)] -> [[(Int)]] -> [[(Int)]]
joinedSetLoop1 initjs [] sl = sl
joinedSetLoop1 initjs js sl  = joinedSetLoop1 initjs (tail js) (joinedSetLoop2 (head js) initjs sl) 

-- assistant-statements for the joinedSetLoop1 Statement
joinedSetLoop2 :: Int -> [(Int)] -> [[(Int)]] -> [[(Int)]]
joinedSetLoop2 i js sl | i == 1 = [js] ++ tail sl
                       | otherwise = [head sl] ++ joinedSetLoop2 (i-1) js (tail sl)
 
 -- take two integer lists and return their union (we treat the lists as sets)
ourUnion :: [(Int)] -> [(Int)] -> [(Int)]
ourUnion l1 l2 = l1 `union` l2

-- access the element of an integers' list with a specific index
accessIndex :: [[(Int)]] -> Int -> [(Int)]
accessIndex [] _ = [(-1)]   -- error
accessIndex l i | i == 1 = head l
                | otherwise = accessIndex (tail l) (i-1)

-- access the element of an integer-tuples' list with a specific index
accessIndexWallList :: [(Int,Int)] -> Int -> [(Int,Int)]
accessIndexWallList [] _ = []
accessIndexWallList l i =
    let
        (ci,cj) = head l
    in
        if ci == i then [(ci,cj)] ++ accessIndexWallList (tail l) i
                    else accessIndexWallList (tail l) i

-- check if an integers' list contains an integer
contains :: [(Int)] -> Int -> Bool
contains [] _ = False
contains l i | head l == i = True
             | otherwise = contains (tail l) i

-- check if a wall exists
containsWall :: [(Int,Int)] -> (Int,Int) -> Bool
containsWall [] _ = False
containsWall l w =
    let
        (a,b) = head l
        (r,d) = w
    in
        if a == r && b == d then True
                            else containsWall (tail l) w

-- initialize sets for Kruskal's Algorithm
sets :: Int -> Int -> [[(Int)]] -- sets is a list of lists including ci
sets w h = [ [x] | x <- [1..w*h] ]

-- initialize walls for Kruskal's Algorithm
walls :: Int -> Int -> [(Int,Int)]
walls w h = [ (ci,cj) | ci <- [1..w*h], cj <- [1..w*h], (cj-ci) == 1, (mod ci w )/=0 ] ++ [ (ci,cj) | ci <- [1..w*h], cj <- [1..w*h], (ci+w) == cj ] 

-- initialize shuffled walls for Kruskal's Algorithm
shuffled_walls :: Int -> Int -> [(Int,Int)]
shuffled_walls w h = shuffle (walls w h)

-- Perfect Maze's Solution Algorithm
solvePerfect :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
solvePerfect m inittup targettup =
    let
        initc = locateCell m inittup
        targetc = locateCell m targettup
        revpath = searchDFS m initc targetc "" True
        path = reverse revpath
        sol = coordPath m path
    in
        sol

-- Assistant-Statements for DFS Solution Algorithm
-- Depth First Search Algorithm for finding the solution of a Perfect Maze
searchDFS :: Maze -> Int -> Int -> String -> Bool -> [Int]
-- "p" is a String ("up"/"down"/"left"/"right") that notes from where we approached the current cell to prevent us from going backwards
-- "can" is a Bool that tells as if we can go to initc (directly) in the first place (meaning no wall)
searchDFS m initc targetc p can | not can = []
                                | initc == targetc = [targetc]
                                | otherwise =
                                    let
                                        up = lookUp m initc
                                        down = lookDown m initc
                                        left = lookLeft m initc
                                        right = lookRight m initc
                                        resUp = (searchDFS m (initc-(width m)) targetc "down" up) ++ [initc]
                                    in
                                        if not (p == "up") && not ((length resUp) == 0) && (head resUp) == targetc then resUp
                                        else let resDown = (searchDFS m (initc+(width m)) targetc "up" down) ++ [initc]
                                             in if not (p == "down") && not ((length resDown) == 0) && (head resDown) == targetc then resDown
                                                else let resLeft = (searchDFS m (initc-1) targetc "right" left) ++ [initc]
                                                     in if not (p == "left") && not ((length resLeft) == 0) && (head resLeft) == targetc then resLeft
                                                        else let resRight = (searchDFS m (initc+1) targetc "left" right) ++ [initc]
                                                             in if not (p == "right") && not ((length resRight) == 0) && (head resRight) == targetc then resRight
                                                                else []

-- turn a path of integers to coordinates
coordPath :: Maze -> [Int] -> [(Int,Int)]
coordPath m pl | length pl == 0 = []
               | otherwise = [locateTuple m (head pl)] ++ coordPath m (tail pl)

-- can we go up?
lookUp :: Maze -> Int -> Bool
lookUp m a = if a > (width m)
                then 
                    let
                        (x,y) = accessCell (cells m) (a-(width m))
                    in
                        not y
                else False

-- can we go down?
lookDown :: Maze -> Int -> Bool
lookDown m a = 
                let
                    (x,y) = accessCell (cells m) a
                in
                    not y

-- can we go left?
lookLeft :: Maze -> Int -> Bool
lookLeft m a = if (mod a (width m)) == 1
                then
                    False
                else
                    let
                        (x,y) = accessCell (cells m) (a-1)
                    in
                        not x

-- can we go right?
lookRight :: Maze -> Int -> Bool
lookRight m a =
                let
                    (x,y) = accessCell (cells m) a
                in
                    not x

-- access the cell of a specific index in the Maze's cells' list
accessCell :: [(Bool,Bool)] -> Int -> (Bool,Bool)
accessCell _ 0 = (False,False)
accessCell cl i | i == 1 = head cl
                | otherwise = accessCell (tail cl) (i-1)

-- turn the coordinate-representation of a cell to a "serial" one like:
-- | 1 | 2 | 3 | ... | maze's width |
-- | 1+maze's width ... |2*maze's width|
locateCell :: Maze -> (Int,Int) -> Int
locateCell m (i,j) = (i-1)*(width m)+j

-- turn the "serial" representation of a cell to a coordinate one
locateTuple :: Maze -> Int -> (Int,Int)
locateTuple m c | (mod c (width m)) == 0 = (div c (width m), width m)   -- last column
                | otherwise = ((div c (width m))+1, mod c (width m))

-- Print Labyrinth and Solution
-- show a maze with a solution applied to one of it's possible paths
showMaze :: Maze -> [(Int, Int)] -> String
showMaze m solTuple =
    let
        sol = cellPath m solTuple
        top = [formTop m 1]
        rows = formRows m sol 1
        mazeList = top ++ rows
    in
        unlines mazeList

-- Assistant-Statements for Printing
-- form the top of the maze's board
formTop :: Maze -> Int -> String
formTop m i | i == (width m) = "+---+"
            | otherwise = "+---" ++ formTop m (i+1)

-- form each row of a maze's board (line and border)
formRows :: Maze -> [Int] -> Int -> [String]
formRows m sol nrow | nrow == (height m)+1 = []
                    | otherwise =
                        let
                            end = nrow * (width m)
                            start = end - (width m) + 1
                            line = formLine m sol start end start
                            border = formBorder m start end start
                        in
                            [line] ++ [border] ++ formRows m sol (nrow+1)

-- form the line of each row of the maze's board
formLine :: Maze -> [Int] -> Int -> Int -> Int -> String
formLine m sol start end i | i == end = formLineElement m sol i
                           | otherwise = (formLineElement m sol i) ++ formLine m sol start end (i+1)

-- form the border of each row of the maze's board
formBorder :: Maze -> Int -> Int -> Int -> String
formBorder m start end i | i == end = formBorderElement m i
                         | otherwise = (formBorderElement m i) ++ formBorder m start end (i+1)

-- form the content that represents best each cell of the maze's board
formLineElement :: Maze -> [Int] -> Int -> String
formLineElement m sol i | ((mod i (width m)) == 1) && not (lookRight m i) = if (contains sol i) then "| * |" else "|   |"
                        | (mod i (width m)) == 1 = if (contains sol i) then "| *  " else "|    "
                        | not (lookRight m i) = if (contains sol i) then " * |" else "   |"
                        | otherwise = if (contains sol i) then " *  " else "    "

-- form the content that represents best the border of each cell of the maze's board
formBorderElement :: Maze -> Int -> String
formBorderElement m i | ((mod i (width m)) == 1) && not (lookDown m i) = "+---+"
                      | not (lookDown m i) = "---+"
                      | (mod i (width m)) == 1 = "+   +"
                      | otherwise = "   +"

-- turn the coordinate representation of a maze's board solution to a "serial" (cells' sequence oriented) one
cellPath :: Maze -> [(Int,Int)] -> [Int]
cellPath m tl | length tl == 0 = []
              | otherwise = [locateCell m (head tl)] ++ cellPath m (tail tl)

--------------------------------------------------------- BONUS !!! ---------------------------------------------------------
-- Produce a Braid maze based on a Perfect maze
-- to form a braid maze we want to remove all the dead-ends of a perfect maze
braid :: Maze -> Maze
braid m = removeDeadEnds m 1

--Assistant-Statements for Braid's labyrinth creation
-- remove the dead-end of each maze's cell that has one
removeDeadEnds :: Maze -> Int -> Maze
removeDeadEnds m i | i == (width m)*(height m) = if isDeadEnd m i then removeOneDeadEnd m i else m
                   | otherwise =
                        if isDeadEnd m i
                        then 
                            let
                                m1 = removeOneDeadEnd m i
                            in
                                removeDeadEnds m1 (i+1)
                        else removeDeadEnds m (i+1)

-- is there a dead-end in the current cell (cell i)?
isDeadEnd :: Maze -> Int -> Bool
isDeadEnd m i =
    let
        possible = [ lookUp m i , lookDown m i , lookLeft m i , lookRight m i ]
        able = filter ( == True ) possible
        numable = length able
    in
        numable == 1    -- if there is only one direction that we can take, then we have reached a dead-end

-- remove (resolve) one cell's dead-end
removeOneDeadEnd :: Maze -> Int -> Maze
removeOneDeadEnd m i | canRemoveRight m i = 
                        let
                            c1 = removeRight (cells m) i
                        in
                            Maze { cells = c1 , width = width m , height = height m }
                     | canRemoveDown m i =
                        let
                            c1 = removeDown (cells m) i
                        in
                            Maze { cells = c1 , width = width m , height = height m }
                     | canRemoveLeft m i =
                        let
                            c1 = removeRight (cells m) (i-1)
                        in
                            Maze { cells = c1 , width = width m , height = height m }
                     | canRemoveUp m i =
                        let
                            c1 = removeDown (cells m) (i-(width m))
                        in
                            Maze { cells = c1 , width = width m , height = height m }
                     | otherwise = m

-- can we remove the wall that is in the right of a cell?
canRemoveRight :: Maze -> Int -> Bool
canRemoveRight m i | (mod i (width m)) == 0 = False
                   | otherwise =
                        let
                            (x,y) = accessCell (cells m) i
                        in
                            True && x   -- if there is no wall Right, there is no point claiming that we can remove it

-- can we remove the wall that is below (down) of a cell?
canRemoveDown :: Maze -> Int -> Bool
canRemoveDown m i | i > (width m)*(height m)-(width m) = False
                  | otherwise =
                        let
                            (x,y) = accessCell (cells m) i
                        in
                            True && y   -- if there is no wall Down, there is no point claiming that we can remove it

-- can we remove the wall that is in the left of a cell?
canRemoveLeft :: Maze -> Int -> Bool
canRemoveLeft m i | (mod i (width m)) == 1 = False
                  | otherwise = canRemoveRight m (i-1)

-- can we remove the wall that is above (up) of a cell?
canRemoveUp :: Maze -> Int -> Bool
canRemoveUp m i | i <= (width m) = False
                | otherwise = canRemoveDown m (i-(width m))

-- remove a cell's right wall
removeRight :: [(Bool,Bool)] -> Int -> [(Bool,Bool)]
removeRight cl i | cl == [] = []
                 | i == 1 =
                    let
                        (x,y) = head cl
                    in
                        [(False,y)] ++ tail cl
                 | otherwise = [head cl] ++ removeRight (tail cl) (i-1)

-- remove a cell's down wall
removeDown :: [(Bool,Bool)] -> Int -> [(Bool,Bool)]
removeDown cl i | cl == [] = []
                | i == 1 =
                    let
                        (x,y) = head cl
                    in
                        [(x,False)] ++ tail cl
                | otherwise = [head cl] ++ removeDown (tail cl) (i-1)

-- Braid Maze's Solution Algorithm
-- solve a Braid Maze using a modified version of the DFS Search that was used to solve a Perfect Maze
solveBraid :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
solveBraid m inittup targettup =
    let
        initc = locateCell m inittup
        targetc = locateCell m targettup
        revpath = searchBraid m initc targetc [] True
        path = reverse revpath
        sol = coordPath m path
    in
        sol

-- Assistant-Statements for Braid Solution Algorithm
-- search the solution of a Braid Maze using a modified version of the DFS Search that was used to solve a Perfect Maze
searchBraid :: Maze -> Int -> Int -> [Int] -> Bool -> [Int]
-- "v" is a [Int] list of the visited cells so far
-- "can" is a Bool that tells as if we can go to initc (directly) in the first place (meaning no wall)
searchBraid m initc targetc v can | contains v initc || not can = []
                                  | initc == targetc = [targetc]
                                  | otherwise =
                                        let
                                            up = lookUp m initc
                                            down = lookDown m initc
                                            left = lookLeft m initc
                                            right = lookRight m initc
                                            resUp = (searchBraid m (initc-(width m)) targetc ([initc]++v) up) ++ [initc]
                                        in
                                            if not ((length resUp) == 0) && (head resUp) == targetc then resUp
                                            else let resDown = (searchBraid m (initc+(width m)) targetc ([initc]++v) down) ++ [initc]
                                                 in if not ((length resDown) == 0) && (head resDown) == targetc then resDown
                                                    else let resLeft = (searchBraid m (initc-1) targetc ([initc]++v) left) ++ [initc]
                                                         in if not ((length resLeft) == 0) && (head resLeft) == targetc then resLeft
                                                            else let resRight = (searchBraid m (initc+1) targetc ([initc]++v) right) ++ [initc]
                                                                 in if not ((length resRight) == 0) && (head resRight) == targetc then resRight
                                                                    else []