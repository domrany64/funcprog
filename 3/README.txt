CS457/557 Functional Programming Summer 2020
Katie Casamento, Portland State University
Homework 3
Due by 11:59pm on Friday, August 14 2020.

0. SUBMISSION INSTRUCTIONS

Submit ONLY your modified Foldables.lhs file. Do not rename the file.
Do not submit a .zip archive.

If your Foldables.lhs file does not compile successfully, it will not be
graded and you will receive a grade of 0 for this assignment.


1. ABOUT THIS PROJECT FOLDER

The folder containing this file is a Cabal project and also a Stack project.
The same commands from HW3 should work:

Cabal:
  Setup:
    cabal update
    cabal install --dependencies-only --enable-tests

  Test:
    cabal test

  Interpreter:
    cabal repl

Stack:
  Setup:
    stack update

  Test:
    stack test

  Interpreter:
    stack ghci
    (you might have to do ":l Foldables" to bring everything into scope)


2. ABOUT THIS HOMEWORK

This homework has you work with several key higher-kinded typeclasses from the
Haskell prelude: Foldable, Functor, and Applicative. In particular, the
Foldable typeclass is defined in terms of a function involving the Monoid
typeclass; much of the content of this assignment can be seen as generalizing
the last assignment's content to obtain versions of "mconcat" and related
operations that work over arbitrary "container" types instead of just lists.
The last question applies all of these concepts to the tic-tac-toe example from
assignment 1, implementing the Board type as a polymorphic container type.
