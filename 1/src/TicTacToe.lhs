This module implements a game of tic-tac-toe, which also goes by other names.
Review the rules on Wikipedia if you're unfamiliar: 
  https://www.wikipedia.org/wiki/Tic-tac-toe

> module TicTacToe where

The code in this module uses a couple imports, but your code won't have to use
any of these.

> import Data.List
> import Data.Ord
> import GHC.Generics

The board in a tic-tac-toe game is 3x3 cells big.
We can define the location (index) of a cell as a pair of coordinates, each
between 0 and 2.

> data Coordinate = C0 | C1 | C2
>   deriving (Eq, Ord, Show, Generic)

The "deriving" part of the above type definition says that expressions of type
Coordinate automatically have an equality operator (==), inequality operators
(<, <=, >, >=, /=), and a function to convert a Coordinate to a String (show).
The Generic typeclass is advanced stuff only used for testing in this code.

An Index is a pair of Coordinates.

> type Index = (Coordinate, Coordinate)

The first coordinate is the X coordinate going from left to right,
and the second coordinate is the Y coordinate going from top to bottom.

  (0,0) (1,0) (2,0)
  (0,1) (1,1) (2,1)
  (0,2) (1,2) (2,2)

Note that we could use (Int,Int) to represent the location of a cell on the
board, but this is more precise: All (terminating) expressions of type Index
are valid cells on a tic-tac-toe board, but e.g. (8,-12345678) is a valid
expression of type (Int,Int) and is not a valid cell.

It will be convenient to have an ascending list of all possible coordinates.

> coordinates :: [Coordinate]
> coordinates = [C0, C1, C2]

A player can win the game by placing three marks in a line. We will represent
these lines by lists of indices.

Here's a function that generates the list of all three indices in a given row:

> rowIxs :: Coordinate -> [Index]
> rowIxs cy = [(C0,cy), (C1,cy), (C2,cy)]

*************
* PROBLEM 1 *  3 points
*************

Fill in the following three definitions so that the spec tests pass.
(Replace the word "undefined" with your definition.)

**BUT**: do not use any of the Coordinate constructors (C0,C1,C2) explicitly.

Instead, use the "coordinates" list by name, and use higher-order list
functions and/or list comprehensions to create lists of indices with it.

Some helpful Prelude functions include:
  map
  zip
  reverse

(Remember to use GHCi to check the types of functions and other expressions!)

The comments above each definition show what the test is expecting. List
elements can be in any order, but each list must not have duplicate elements.

> -- columnIxs cx = [(cx,C0), (cx,C1), (cx,C2)]
> columnIxs :: Coordinate -> [Index]
> columnIxs cx = zip (repeat cx) coordinates 

> -- downDiagIxs = [(C0,C0), (C1,C1), (C2,C2)]
> downDiagIxs :: [Index]
> downDiagIxs = zip coordinates coordinates
> -- downDiagIxs = [(cx,cy) | cx <- coordinates, cy <- coordinates, cx == cy]

> -- upDiagIxs = [(C0,C2), (C1,C1), (C2,C0)]
> upDiagIxs :: [Index]
> upDiagIxs = zip coordinates (reverse coordinates)

*****************
* END PROBLEM 1 *
*****************


winLines combines the lines defined above into a list of all possible ways to
win the game.

> winLines :: [[Index]]
> winLines =
>   downDiagIxs :
>   upDiagIxs :
>   map columnIxs coordinates ++
>   map rowIxs coordinates

boardRows is a list of rows that represents the whole board; this is useful
when printing the board.

> boardRows :: [[Index]]
> boardRows = map rowIxs coordinates

allIxs is a list of all of the indices in the board.

> allIxs :: [Index]
> allIxs = concat boardRows


In the implementation at the bottom of this file, the user is always X and goes first, and the computer is always O and goes second.

> data Player = X | O
>   deriving (Eq, Show)

> opponent :: Player -> Player
> opponent X = O
> opponent O = X

A value of type Cell is the state of some cell on a game board: either a player
has put a mark in the cell, or the cell is empty.

> data Cell = Mark Player | Empty
>   deriving Eq

Here we give an instance of the Show typeclass for Cell. We'll talk about
typeclasses in lecture soon, but for now all you need to know is that this
special way of defining the "show" function for the Cell type lets GHCi print
Cell values the way we tell it to.

> instance Show Cell where
>   show (Mark a) = show a
>   show Empty = "."

A Board is a function from an Index to a Cell. This may seem strange, but a
function with a finite input type acts almost exactly like a container data
structure.

The definition below uses Haskell record syntax. It generates two
expression-level names: the "Board" constructor and the "cell" function. Check
the types of both of them in GHCi!
  
> data Board = Board { cell :: Index -> Cell }

To convert a Board to human-readable text representation, we map a function
over each possible index to show the cell at that index.

> instance Show Board where
>   show b =
>     unlines (map (concat . intersperse " " . map (show . cell b)) boardRows)

Here's an example board to illustrate how to define boards for testing.

> example :: Index -> Cell
> example (C0,C0) = Mark X; example (C1,C0) = Mark O; example (C2,C0) = Empty
> example (C0,C1) = Mark X; example (C1,C1) = Mark X; example (C2,C1) = Empty
> example (C0,C2) = Mark O; example (C1,C2) = Empty; example (C2,C2) = Mark O

> exampleBoard :: Board
> exampleBoard = Board example

Note that GHCi will complain if you try to evaluate "example", but
"exampleBoard" will print correctly - this is why we created a special Board
type wrapping a function type instead of just using a function type directly.

An empty board is a board that is empty at all indices.

> emptyBoard :: Board
> emptyBoard = Board (const Empty)

The "playerAt" function returns a Bool indicating whether a player's mark is in
a given cell.

> playerAt :: Board -> Player -> Index -> Bool
> playerAt b x i = cell b i == Mark x

The emptyAt function tests whether a given index on a board is empty.

> emptyAt :: Board -> Index -> Bool
> emptyAt b i = cell b i == Empty

*************
* PROBLEM 2 *  2 points
*************

Give a definition for the emptyIxs function so that it returns the list of all indices on a board contain the Cell value Empty.

As in problem 1, do not use the Coordinate constructors explicitly; use
previously defined functions and lists to filter the list of all indices down
to only those that contain an empty cell.

> emptyIxs :: Board -> [Index]
> emptyIxs b = [(x,y) | x <- coordinates, y <- coordinates, emptyAt b (x,y)]

*****************
* END PROBLEM 2 *
*****************

*************
* PROBLEM 3 *  3 points
*************

Give a definition for the "won" function so that it returns a boolean value indicating whether the given player has won the game on the given board.

As in problems 1 and 2, do not use the Coordinate constructors explicitly.
Instead, use previous definitions from this file along with higher-order list
functions. You may add new definitions above the definition of "won" if you
want, but it is not necessary for a solution.

These functions from the Prelude will be helpful:
  any :: (a -> Bool) -> [a] -> Bool
  all :: (a -> Bool) -> [a] -> Bool

GHCi will report very general types for them involving the Foldable typeclass,
but for now you should treat them as having the above types. The function call
"any f xs" tests whether "f" returns True for *at least one* element of "xs",
and "all f xs" tests whether "f" returns True for *every* element of "xs".

> won :: Board -> Player -> Bool
> won b x = any (all (playerAt b x)) winLines 

*****************
* END PROBLEM 3 *
*****************


The "inProgress" function decides whether a game is still in progress.
A game is in progress when there is at least one empty space and neither player
has won yet.

> inProgress :: Board -> Bool
> inProgress b = not (won b X || won b O) && any (emptyAt b) allIxs

The "write" function updates a board by placing a given player's mark at a
given index. Note that if the given index is not empty, the function does
nothing; this is not the best form of error handling, and later in class we'll
see more principled ways of dealing with functions that may fail.

(Try using this function in GHCi!)

> write :: Index -> Player -> Board -> Board
> write i x b =
>   Board $ \i' ->
>     if i == i' && emptyAt b i then
>       Mark x
>     else
>       cell b i'

This is everything we need to play tic-tac-toe. Try it out in GHCi!

Now let's write an AI to play against!

First, we'll define the type of game outcomes.

> data Outcome = Loss | Tie | Win
>   deriving (Eq, Ord, Show)

Under the "deriving Ord" feature, the constructors are automatically defined as
increasing from left to right:
  Loss < Tie
  Tie  < Win

The "opponentOutcome" function flips an Outcome, to represent the outcome for
the opposing player: if one player has won, the opposing player has lost.

> opponentOutcome :: Outcome -> Outcome
> opponentOutcome Loss = Win
> opponentOutcome Tie = Tie
> opponentOutcome Win = Loss

The "boardOutcome" function computes the best outcome a given player can achieve
from a given board state.

This is a minimax algorithm from the field of AI, although this is an almost
trivial application of the concept. Don't worry if you're not familiar with the
algorithm - you don't need to understand why this function works in order to
solve the next problem.

This is an extremely inefficient AI, but it's very concise!

> boardOutcome :: Board -> Player -> Outcome
> boardOutcome b x =
>   if won b x then Win
>   else if won b (opponent x) then Loss
>   else if not (inProgress b) then Tie
>   else maximum (map (moveOutcome b x) (emptyIxs b))


*************
* PROBLEM 4 *  2 points
*************

Give a definition for the "moveOutcome" function, using "boardOutcome".
Your definition should implement this algorithm:
  - create a new board by making a mark for player x on board b at index i
  - call "boardOutcome" to get the new board outcome for *the opponent* of x
  - call "opponentOutcome" on that result to get the board outcome for x

Your definition should not be directly recursive - calling "boardOutcome" will
recurse into "moveOutcome", so "moveOutcome" doesn't need to have any calls to
"moveOutcome".

Replace the "Tie" below with your definition.

> moveOutcome :: Board -> Player -> Index -> Outcome
> moveOutcome b x i = Tie

*****************
* END PROBLEM 4 *
*****************


Now, the payoff for your work is a function that returns the best move for a
given player on a given board. Note that this throws a runtime error if there
are no empty spaces, so callers of this function should call "inProgress" first
to check if a game is still going.

> aiMove :: Board -> Player -> Index
> aiMove b x = maximumBy (comparing (moveOutcome b x)) (emptyIxs b)


The rest of the code in this file implements the IO part of a console
tic-tac-toe game. Don't worry about understanding this code for now, but feel
free to ask me about it if you want to understand it!

> readCoord :: Char -> Maybe Coordinate
> readCoord '0' = Just C0
> readCoord '1' = Just C1
> readCoord '2' = Just C2
> readCoord _ = Nothing

> playerAct :: Board -> Player -> IO Board
> playerAct b x = do
>   input <- getLine
>   let tryAgain msg = putStrLn msg >> playerAct b x
>   case input of
>     [cx, ' ', cy] ->
>       case (readCoord cx, readCoord cy) of
>         (Just cx', Just cy') -> let i = (cx',cy') in
>           if emptyAt b i then return $ write i x b
>           else tryAgain "illegal move"
>         (Nothing, _) -> tryAgain "invalid input on first coordinate"
>         (_, Nothing) -> tryAgain "invalid input on second coordinate"
>     _ -> tryAgain "invalid input"

> aiAct :: Board -> Player -> Board
> aiAct b x = write (aiMove b x) x b

> exitMsg :: Board -> IO ()
> exitMsg b = do
>   if won b X then putStrLn "X wins!"
>   else if won b O then putStrLn "O wins!"
>   else putStrLn "it's a tie"

> play :: Board -> IO ()
> play b = do
>   print b
>   if inProgress b then do
>     b' <- playerAct b X
>     print b'
>     if inProgress b' then
>       play $ aiAct b' O
>     else
>       exitMsg b'
>   else
>     exitMsg b
