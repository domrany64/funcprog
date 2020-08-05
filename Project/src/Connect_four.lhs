This module implements a game of "connect four", which also goes by other names.
Review the rules on Wikipedia if you're unfamiliar:
https://en.wikipedia.org/wiki/Connect_Four

> module Connect_four where
> import Data.List
> import Data.Ord
> import GHC.Generics

The board in a "connect four" game is 7x6 cells big.
We can define the location (index) of a cell as a pair of coordinates, each
between 0 and 2.

> data CoordinateY = Y0 | Y1 | Y2 | Y3 | Y4 | Y5
>   deriving (Eq, Ord, Show, Generic)
> data CoordinateX = X0 | X1 | X2 | X3 | X4 | X5 | X6
>   deriving (Eq, Ord, Show, Generic)
> type Index = (CoordinateX, CoordinateY)

The first coordinate is the X coordinate going from left to right,
and the second coordinate is the Y coordinate going from top to bottom.

  (0,0) (1,0) (2,0) (3,0) (4,0) (5,0) (6,0)
  (0,1) (1,1) (2,1) (3,1) (4,1) (5,1) (6,1)
  (0,2) (1,2) (2,2) (3,2) (4,2) (5,2) (6,2)
  (0,3) (1,3) (2,3) (3,3) (4,3) (5,3) (6,3)
  (0,4) (1,4) (2,4) (3,4) (4,4) (5,4) (6,4)
  (0,5) (1,5) (2,5) (3,5) (4,5) (5,5) (6,5)

> rows :: [CoordinateY]
> rows = [Y0, Y1, Y2, Y3, Y4, Y5]

> columns :: [CoordinateX]
> columns = [X0, X1, X2, X3, X4, X5, X6]

> rowIxs :: CoordinateY -> [Index]
> rowIxs cy = zip columns (repeat cy)

> columnIxs :: CoordinateX -> [Index]
> columnIxs cx = zip (repeat cx) rows

> boardRows :: [[Index]]
> boardRows = map rowIxs rows

> allIxs :: [Index]
> allIxs = concat boardRows

> data Player = X | O
>   deriving (Eq, Show)

> opponent :: Player -> Player
> opponent X = O
> opponent O = X

> data Cell = Mark Player | Empty
>   deriving Eq

> instance Show Cell where
>   show (Mark a) = show a
>   show Empty = "."

> data Board = Board { cell :: Index -> Cell }

> instance Show Board where
>   show b =
>     unlines (map (concat . intersperse " " . map (show . cell b)) boardRows)

> emptyBoard :: Board
> emptyBoard = Board (const Empty)

> playerAt :: Board -> Player -> Index -> Bool
> playerAt b x i = cell b i == Mark x

> emptyAt :: Board -> Index -> Bool
> emptyAt b i = cell b i == Empty

  extract function is to get the integer value from a maybe value.

> extract :: Maybe Int -> Int 
> extract mx = case mx of
>   Just x -> x
>   Nothing -> -1 

  extractIdx does the same job for the indices.
  Note that "Nothing" happens when there is no room left in the column which I'm dealing with in playerAct function.

> extractIdx :: Maybe Index -> Index
> extractIdx idx = case idx of
>   Just idx -> idx
>   Nothing -> (X0,Y0)

  xtoIndex is to give the first empty cell of a column from bottom.
  In the real world board game this job is done by the gravity!!

> xtoIndex :: Board -> CoordinateX -> Maybe Index
> xtoIndex b x = do
>		   let res = elemIndex True (map (\y -> emptyAt b (x,y)) (reverse rows))
>                  case res of
> 	  	     Just _ -> Just (x,(reverse rows !! extract res))
>   		     Nothing -> Nothing

> write :: Index -> Player -> Board -> Board
> write i x b =
>   Board $ \i' ->
>     if i == i' && emptyAt b i then
>       Mark x
>     else
>       cell b i'

  The next four variables are generating the list of every possible winning case.
  Afterward, they are all merged to the "winLines" by the use of a custom "append" function.

> fourinColumn :: [[Index]]
> fourinColumn = [(zip (repeat x) y) | x <- columns, y <- [take 4 (drop y rows) | y <- [0.. (length rows - 4)]]]

> fourinRow :: [[Index]]
> fourinRow = [(zip  x (repeat y)) | x <- [take 4 (drop x columns) | x <- [0.. (length columns - 4)]], y <- rows]

> diagR :: [[Index]]
> diagR = [[((columns !! x),(rows !! y)), ((columns !! (x+1)),(rows !! (y+1))), ((columns !! (x+2)),(rows !! (y+2))), ((columns !! (x+3)),(rows !! (y+3)))] | x <- [0 .. ((length columns) - 1)], x+3 < (length columns), y <-  [0 .. ((length rows) - 1)], y+3 < (length rows)]

> diagL = [[((columns !! x),(rows !! y)), ((columns !! (x+1)),(rows !! (y-1))), ((columns !! (x+2)),(rows !! (y-2))), ((columns !! (x+3)),(rows !! (y-3)))] | x <- [0 .. ((length columns) - 1)], x+3 < (length columns), y <-  [0 .. ((length rows) - 1)], y-3 > -1]

> append :: [a] -> [a] -> [a]
> append xs ys = foldr (:) ys xs

> winLines :: [[Index]]
> winLines =  append (append (append diagL diagR) fourinColumn) fourinRow

> won :: Board -> Player -> Bool
> won b x = any (all (playerAt b x)) winLines

> data Outcome = Loss | Tie | Win
>   deriving (Eq, Ord, Show)

> opponentOutcome :: Outcome -> Outcome
> opponentOutcome Loss = Win
> opponentOutcome Tie = Tie
> opponentOutcome Win = Loss

> inProgress :: Board -> Bool
> inProgress b = not (won b X || won b O) && any (emptyAt b) allIxs

  exampleBoard is to use during test.

> example :: Index -> Cell
> example (X0,Y0) = Empty; example (X1,Y0) = Empty; example (X2,Y0) = Mark X; example (X3,Y0) = Empty; example (X4,Y0) = Empty; example (X5,Y0) = Empty; example (X6,Y0) = Empty
> example (X0,Y1) = Empty; example (X1,Y1) = Empty; example (X2,Y1) = Mark O; example (X3,Y1) = Empty; example (X4,Y1) = Empty; example (X5,Y1) = Empty; example (X6,Y1) = Empty
> example (X0,Y2) = Empty; example (X1,Y2) = Empty; example (X2,Y2) = Mark X; example (X3,Y2) = Empty; example (X4,Y2) = Empty; example (X5,Y2) = Empty; example (X6,Y2) = Empty
> example (X0,Y3) = Empty; example (X1,Y3) = Empty; example (X2,Y3) = Mark O; example (X3,Y3) = Empty; example (X4,Y3) = Empty; example (X5,Y3) = Empty; example (X6,Y3) = Mark O
> example (X0,Y4) = Empty; example (X1,Y4) = Empty; example (X2,Y4) = Mark X; example (X3,Y4) = Empty; example (X4,Y4) = Empty; example (X5,Y4) = Empty; example (X6,Y4) = Mark O
> example (X0,Y5) = Empty; example (X1,Y5) = Empty; example (X2,Y5) = Mark O; example (X3,Y5) = Mark X; example (X4,Y5) = Mark X; example (X5,Y5) = Mark X; example (X6,Y5) = Mark O
> exampleBoard :: Board
> exampleBoard = Board example

  This function is to check whether a column is full or still can be used.

> notFullColumns :: Board -> [CoordinateX]
> notFullColumns b = [x | x <- columns, ((xtoIndex b x) /= Nothing)]

  There are 7 different columns in the standard "Connect four" board game,
  A player can choose one of these columns by its number to drop the marker in.

> readCoord :: Char -> Maybe CoordinateX
> readCoord '0' = Just X0
> readCoord '1' = Just X1
> readCoord '2' = Just X2
> readCoord '3' = Just X3
> readCoord '4' = Just X4
> readCoord '5' = Just X5
> readCoord '6' = Just X6
> readCoord _ = Nothing

  playerAct is to check if the move is valid, if so does the move, if not shows a message to the player.

> playerAct :: Board -> Player -> IO Board
> playerAct b x = do
>   input <- getLine
>   let tryAgain msg = putStrLn msg >> playerAct b x
>   case input of
>     [cx] ->
>       case readCoord cx of
>         Just cx' -> let i = cx' in
>           if (cx' `elem` notFullColumns b) then return $ write (extractIdx (xtoIndex b cx')) x b
>           else tryAgain "Column is full, use another"
>         Nothing  -> tryAgain "invalid column number"
>     _ -> tryAgain "invalid input"

> exitMsg :: Board -> IO ()
> exitMsg b = do
>   if won b X then putStrLn "X wins!"
>   else if won b O then putStrLn "O wins!"
>   else putStrLn "it's a tie"

> play :: Board -> IO ()
> play b = do
>   if inProgress b then do
>     b' <- playerAct b X
>     print b'
>     if inProgress b' then do
>      b'' <- playerAct b' O
>      print b''
>      play b''
>     else
>       exitMsg b'
>   else
>     exitMsg b
