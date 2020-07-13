CS457/557 Functional Programming Summer 2020
Katie Casamento, Portland State University
Homework 1

Due before 11:59pm on Sunday, July 5th.

0. SUBMISSION INSTRUCTIONS

Submit ONLY your modified TicTacToe.lhs file. Do not rename the file.
Do not submit a .zip archive.

If your TicTacToe.lhs file does not compile successfully, it will not be
graded and you will receive a grade of 0 for this assignment.


1. ABOUT THIS PROJECT FOLDER

The folder containing this file is a Cabal project. Cabal is a standard build
system for Haskell, with an integrated package manager for resolving library
dependencies. (You can also use a similar build tool called "stack" if you
prefer, but Cabal comes pre-installed with the Haskell Platform.)

The file hw1.cabal defines the structure of the project: there is a library
called "hw1" that contains the file src/TicTacToe.lhs, an executable
"tictactoe" that contains the file src/Main.hs, and a test executable "spec"
that contains the file test/Spec.hs, which automatically discovers and executes
the tests in test/TicTacToeSpec.hs.

You won't be required to create or modify .cabal files in this course, but they
might be useful in your course project!


2. ABOUT THIS HOMEWORK

In this assignment, you'll be filling in a few definitions in an implementation
of a tic-tac-toe game.

The file src/TicTacToe.lhs has four problems for you to work through. There are
10 total points in the assignment. Code with compilation errors will not be
graded and will receive an automatic zero!

Working with a Cabal project requires a couple extra steps, so you won't be
using the "ghci" command directly. First, run these commands to download and
install the testing framework from Hackage, the Haskell package manager:

  cabal update
  cabal install --dependencies-only --enable-tests

If you get an error, send me a Slack message or email ASAP and I'll help debug!

To run GHCi, run this command:
  
  cabal repl

To run the test suite, run this command:

  cabal test

Feel free to comment out tests and re-enable tests as you go if you want
cleaner test output: you just have to comment out one or more of the blocks
that start with "prop" in test/TicTacToeSpec.hs (except there has to be at
least one uncommented).

The test suite is implemented with the QuickCheck library, which does *property
testing*: the test code we write specifies some algebraic properties of the
functions we're testing, and the QuickCheck library automatically tries to come
up with inputs that fail our tests. This is kind of silly in the first two
tests in TicTacToeSpec.hs, where there are only three possible inputs to the
functions, but the approach really shines through in the last three tests.

In the tests for the "emptyIxs" and "won" functions, I've defined
implementations that are about as easy as possible to verify by eye, and
QuickCheck tries to come up with inputs that act differently as input to these
"obvious" functions than as input to your implementations. (The assignment text
requires yours to be a little more concise than the "obvious" implementations.)

In the last test, QuickCheck tries to beat an AI at tic-tac-toe!
(This test might take a minute or two, but shouldn't take much longer.)


3. PLAYING TIC-TAC-TOE

Once you've completed the first three problems, you can play tic-tac-toe in
your console against an AI. Run these commands:

  cabal build
  cabal run tictactoe

Enter your move as two numbers separated by a space: the first number is the
column of your move, and the second is the row of your move. (Don't enter them
in the Coordinate/Index syntax used within TicTacToe.hs, just enter the numbers
themselves.)

The AI should play a game successfully as soon as you've finished the first
three problems (as soon as there are no occurrences of "undefined" in the
program), but it might lose games if you haven't gotten the correct solution to
the fourth problem yet. This AI is extremely inefficient, but very clean to
implement!
