CS457/557 Functional Programming Summer 2020
Katie Casamento, Portland State University
Homework 2

Due before 11:59pm on Monday, July 20.

0. SUBMISSION INSTRUCTIONS

Submit ONLY your modified Monoids.lhs file. Do not rename the file.
Do not submit a .zip archive.

If your Monoids.lhs file does not compile successfully, it will not be
graded and you will receive a grade of 0 for this assignment.

1. ABOUT THIS PROJECT FOLDER

The folder containing this file is a Cabal project and also a Stack project.

If you're using Cabal, use the same commands as last time: initialize with
  cabal update && cabal install --dependencies-only --enable-tests
and then run the interpreter with "cabal repl" and test with "cabal test".
(You might need to use "cabal new-build" instead of cabal install", depending
on the version of Cabal on your system.)

If you're using Stack, you don't need to initialize anything; run the
interpreter with "stack ghci" and test with "stack test".


Let me know if you have any build issues!


2. ABOUT THIS HOMEWORK

In this assignment you'll be working with the Monoid typeclass, which is one
of the standard classes defined in the Prelude.
