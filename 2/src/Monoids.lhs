> {-# LANGUAGE NoImplicitPrelude #-}

> module Monoids where

This assignment has you work with the Monoid typeclass.

The Haskell community has a general preference for borrowing names from
abstract mathematics, which is often intimidating to beginners: if the name
"Monoid" doesn't mean anything to you yet, don't worry! Try not to be scared
of the new terminology: most of this material is much simpler than it appears
at first.

The first thing to keep in mind is that Monoid, as a typeclass, is like an
adjective that applies to types. When we say "type X is a Monoid", it should
be interpreted as "it is possible to implement a lawful instance of the
Monoid typeclass for type X". (We'll review what it means for an instance to
be "lawful" further below.) 

Here's the intuition from a programming perspective:

  A type X is a monoid if we can "join" or "concatenate" any collection of
  elements of type X to obtain a single element, using a divide-and-conquer
  recursion pattern.

In the naming tradition of OOP interfaces, Monoid might be called something
like "Combinable".

The Monoid typeclass is part of the Prelude, but we'll define it manually
in this file, so this import hides the default definition along with some other standard library types and functions that we'll be re-defining.

> import Prelude hiding
>   ( (<>), Monoid(..), Sum(..), Product(..), First(..)
>   , any, all, foldMap, sum, product
>   )

This import is just for magic test generation stuff, like last time.

> import GHC.Generics

This is for a debugging function that's helpful in problem 4; we'll cover what
it means soon.

> import Control.Monad

Here's the definition of the Monoid class. The "infixr 6" line says that
the <> operator is parsed as a right-associative operator with precedence 6.

> class Monoid a where
>   infixr 6 <>
>
>   -- associativity: x <> (y <> z) == (x <> y) <> z
>   (<>) :: a -> a -> a
>
>   -- left unit:  mempty <> x == x
>   -- right unit: x <> mempty == x
>   mempty :: a

You can get information about the Monoid typeclass in GHCi with the command
":i Monoid".

The <> operator is how we "join" or "concatenate" two elements together.
This operator is traditionally called "mappend" in Haskell, but the laws are
easier to read if we use an infix operator instead.

The name "mempty" is short for "monoid empty", and represents the value that we
get when we "join" or "concatenate" an empty collection of elements.

The "associativity" and "left/right unit" comments are *laws*: they say that an
instance of Monoid is only valid if the given equations hold for all choices of
x/y/z. Haskell's type system isn't strong enough to allow us to statically
require that laws hold, so we have to check them manually with tests and/or
pen-and-paper proofs.

(There are languages with type systems that can enforce laws: see Agda, Idris,
Lean, Coq, ATS, or the LiquidHaskell compiler plugin for Haskell.)

Notice that the law uses the == operator, which is from the Eq typeclass.
Equality in typeclass laws is actually a little more subtle than this; when a
type has an Eq instance we expect the law to be true according to the ==
operator, but as we'll see below, there are types that have instances of
Monoid but not Eq.

In general, typeclass laws are expected to hold up to some well-defined
type-specific notion of equality: the two sides may not be *identical*
expressions, but they should represent the "same" value in some way.
In particular, equality over values of function type is handled in a special
way when checking laws, which comes up in problem 1c and is discussed there.


Let's start with the most boring monoid: the unit type. Recall that the
syntax () at the type level denotes the unit type, and the same syntax at
term level denotes the unit term:

> unitExample :: ()
> unitExample = ()

There's only one value of the unit type, so there's only one possible
implementation for <> and mempty.

> instance Monoid () where
>   x <> y = ()
>   mempty = ()

We can easily check the associative law with case-by-case equational reasoning.
There's only one possible case: x, y, and z must all have the value ().
  
    () <> (() <> ())
  = () <> ()
  = (() <> ()) <> ()


What about Bool? There's more than one lawful choice for the <> operator:
the AND (&&) and OR (||) functions are both asssociative, and both have a
"unit" value (True for && and False for ||).

(Mathematically, the triples <Bool,(&&),True> and <Bool,(||),False> are both
valid monoids.)

The traditional Haskell way to resolve this issue is to declare two new types
that "wrap" the Bool type, so that we can define two different Monoid
instances with the same underlying data representation.

The "newtype" keyword is logically equivalent to the "data" keyword except that it can only be used for data types that have exactly one single-argument constructor and no other constructors.

The reason for this restriction is that a type declared with "newtype" is more efficient than one declared with "data": at runtime, there is no difference between the All type defined below and the Bool type (newtypes are zero-cost). If we defined the All type with "data" isntead of "newtype", the All type would be less efficient than the Bool type (it would require an extra allocation).

(GHC is probably smart enough to optimize away this extra cost even for types
declared with "data", but using "newtype" guarantees this optimization.)

> -- booleans with AND
> newtype All = All { getAll :: Bool }
>   deriving (Show, Eq, Generic)

Recall that record syntax in a datatype declaration creates both a constructor
and a field extraction function: check the types of the "All" and "getAll"
functions in GHCi. In this case, the only point of using the record syntax is
to get an automatic definition for the "getAll" function, which is very boring
to write by hand.

The All type represents "booleans with &&", so we define <> to be && and mempty
to be True. Note that we have to explicitly wrap and unwrap the All values in
this definition, but at runtime these are guaranteed to be no-ops because All
is a newtype.

> instance Monoid All where
>   x <> y = All (getAll x && getAll y)
>   mempty = All True

We can prove the Monoid laws for All with case-by-case reasoning.


Similarly, the Any type represents "booleans with ||".

> -- booleans with OR
> newtype Any = Any { getAny :: Bool }
>   deriving (Show, Eq, Generic)

> instance Monoid Any where
>   x <> y = Any (getAny x || getAny y)
>   mempty = Any False

Note that Haskell will not notice if we mix up the Monoid definitions for All
and Any. The intended meaning of the wrapper types is given in comments, and
the computational meaning is given by the instance definitions for the type;
the programmer has to make sure these two meanings agree.

This isn't very exciting yet, but now we can use the <> operator in slightly
nontrivial expressions: try these out in GHCi!

  Any True <> Any False
  getAny (Any True <> Any False)
  All True <> All False
  getAll (All True <> All False)
  Any True <> All False -- think about why this fails

A pair type is a monoid when both of its element types are monoids.
Recall that the double arrow (=>) specifies a *constraint* on this instance:
this declares an instance of Monoid (a,b) for exactly all pairs of types
a/b where each type has a Monoid instance.

> instance (Monoid a, Monoid b) => Monoid (a,b) where
>   (x1,y1) <> (x2,y2) = (x1<>x2, y1<>y2)
>   mempty = (mempty, mempty)

This is a lawful instance as long as the instances for "a" and "b" are lawful.


A list type of any element type is a monoid, where <> is list append and mempty
is the empty list.

> instance Monoid [a] where
>   (<>) = (++)
>   mempty = []

Note that this instance does *not* require that "a" is a monoid.
This instance can be proven lawful using a technique called "structural
induction", which is outside the scope of this assignment but will be covered
in lecture at some point.


*************
* PROBLEM 1 *  3 points
*************

Give lawful definitions for each of the following Monoid instances.

Do not modify any code outside of the Monoid instance definitions.
You may add extra cases and arguments to the method definitions in the Monoid
instance definitions.

There may be multiple different valid answers for each question.


a.

> instance Monoid a => Monoid (Maybe a) where
>   Just x <> Just y = Just (x <> y)
>   Nothing <> y = y
>   x <> Nothing = x
>   mempty = Nothing


b.

> data These a b = This a | That b | These a b
>   deriving (Show, Generic)

> instance (Monoid a, Monoid b, Eq a, Eq b) => Eq (These a b) where
>   This  x1    == This  x2    = x1 == x2
>   That     y1 == That     y2 = y1 == y2
>   These x1 y1 == These x2 y2 = x1 == x2 && y1 == y2
>   This  x1    == These x2 y2 = x1 == x2 && y2 == mempty
>   That     y1 == These x2 y2 = y1 == y2 && x2 == mempty
>   These x1 y1 == This  x2    = x1 == x2 && y1 == mempty
>   These x1 y1 == That     y2 = y1 == y2 && x1 == mempty
>   This  x1    == That     y2 = False
>   That     y1 == This  x2    = False

> instance (Monoid a, Monoid b) => Monoid (These a b) where
>   These x1 y1 <> These x2 y2 = These (x1<>x2) (y1<>y2)
>   This  x1    <> These x2 y2 = These (x1 <> x2) (mempty <> y2)
>   That     y1 <> These x2 y2 = These (mempty <> x2) (y1 <> y2)
>   These x1 y1 <> This  x2    = These (x1 <> x2) (y1 <> mempty)
>   These x1 y1 <> That     y2 = These (x1 <> mempty) (y1 <> y2)
>   This  x1    <> This  x2    = This (x1 <> x2)
>   That     y1 <> That     y2 = That (y1 <> y2)
>   This  x1    <> That     y2 = These x1 y2
>   That     y1 <> This  x2    = These x2 y1 
>   mempty = These mempty mempty

c.

We can't actually define an Eq instance for function types, so this instance
should satisfy the associativity law when "f == g" over functions f/g means
"for any valid input x, f x == g x".

> instance Monoid b => Monoid (a -> b) where
>   f <> g = \x -> (f x) <> (g x) 
>   mempty = \x -> mempty


*****************
* END PROBLEM 1 *
*****************


Consider the following type, a binary tree type with data at each node.

> data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
>   deriving Generic


The showTree function can be used to show BinTrees in a more visual format.

> showTree :: Show a => Int -> BinTree a -> String
> showTree indent Leaf = replicate indent ' ' ++ "."
> showTree indent (Node x l r) =
>   concat
>     [ replicate indent ' ', show x, "\n"
>     , showTree (2+indent) l, "\n"
>     , showTree (2+indent) r
>     ]

This instance makes showTree the default formatter when GHCi prints a BinTree.

> instance Show a => Show (BinTree a) where
>   show = showTree 0

The nodeCount function returns the number of non-leaf nodes in a BinTree.

> nodeCount :: BinTree a -> Int
> nodeCount Leaf = 0
> nodeCount (Node _ l r) = 1 + nodeCount l + nodeCount r

The nodeAt function retrieves the value of the node at the given index in the
ordering of a preorder traversal. (Try out some test trees in GHCi to make sure
you understand what this means!)

> nodeAt :: Int -> BinTree a -> Maybe a
> nodeAt _ Leaf = Nothing
> nodeAt i (Node x l r) =
>   let midpoint = nodeCount l in
>     if i == 0 then
>       Just x
>     else if i <= midpoint then
>       nodeAt (i - 1) l
>     else
>       nodeAt (i - (1 + midpoint)) r

Here's an example test tree to play around with, where the nodes in the
ordering of a preorder traversal are in alphabetical order.

> testTree :: BinTree Char
> testTree =
>   Node 'a'
>     (Node 'b'
>       (Node 'c'
>         Leaf
>         (Node 'd' Leaf Leaf))
>       (Node 'e'
>         (Node 'f' Leaf Leaf)
>         (Node 'g'
>           (Node 'h' Leaf Leaf)
>           (Node 'i' Leaf Leaf))))
>     Leaf

The nodeList function returns a list of the values of all the nodes in a
BinTree in the ordering of a preorder traversal.

> nodeList :: BinTree a -> [a]
> nodeList Leaf = []
> nodeList (Node x l r) = x : nodeList l ++ nodeList r


*************
* PROBLEM 2 *  2 points
*************

This instance of Monoid for BinTree passes the Haskell typechecker.

> instance Monoid (BinTree a) where
>   t1 <> Leaf = t1
>   Leaf <> t2 = t2
>   Node x1 l1 r1 <> t2 = Node x1 (l1 <> r1) t2
>
>   mempty = Leaf

It is not necessarily a valid instance, however: recall that the monoid laws
depend on the implementation of the Eq typeclass for BinTree.

Give a definition for == in the following Eq instance that, together with the
Monoid instance given above, obeys all of these laws:

  - the Monoid laws: associativity and left/right unit
  - index-wise equality:
      for any trees t1/t2,
      for any integer i,
        (t1 == t2) implies (nodeAt i t1 == nodeAt i t2)
  - the Eq laws:
    - reflexivity: x == x
    - symmetry: (x == y) implies (y == x)
    - transitivity: (x == y and y == z) implies (x == z)

Focus on the Monoid laws and the index-wise equality law; the Eq laws will
mostly follow from those.

You may modify the existing definition code for == to add parameters and
pattern-matching cases.

The definition of /= is automatically generated from your definition of ==.
Do not modify the Monoid instance for BinTree above.

Don't worry about efficiency. This is probably simpler than you think!

> instance Eq a => Eq (BinTree a) where
>   t1 == t2 = (nodeList t1 == nodeList t2)

*****************
* END PROBLEM 2 *
*****************


We can write definitions that are *parametric* over the choice of monoid,
meaning they work for any type with a Monoid instance.

Here's a straightforward example: given three elements of the same monoid,
we can use <> to concatenate them all together.

Recall that the => operator in a type is a *constraint*: this function takes in
three inputs of the same arbitrary type "a" and returns an output of type "a",
where "a" must have a Monoid instance.

> mconcat3 :: Monoid a => a -> a -> a -> a
> mconcat3 a b c = a <> b <> c

(The name "mconcat" is short for "monoid concatenate".)

We can generalize this to any number of elements: the "mconcat" function joins
a list of elements together, as long as the type of elements is a monoid.
Note that the "mempty" value is used in the case of an empty list, because we
have to do something in this case!

> mconcat :: Monoid a => [a] -> a
> mconcat [] = mempty
> mconcat (x : xs) = x <> mconcat xs


The mconcat function gets a kind of "summary" of a collection of elements. The
Monoid instance for the type of the list elements is what decides exactly which
kind of "summary" we get.

For example, mconcat with a list of Any values computes a result indicating
whether there is at least one True value in a collection: try it in GHCi with
an expression like "mconcat [Any True, Any False, Any False]" (without the
quotes). We can use this to derive the standard "any" function, which operates
over a list of Bool values instead of Any values.

> any :: [Bool] -> Bool
> any bs = getAny (mconcat (map Any bs))

Since Any is a newtype, GHC guarantees that the "getAny" and "map Any" calls are
fully optimized away, and this code compiles down to a single recursive pass
over the input list with no unnecessary allocations.

This pattern is common enough that there is a standard function for it.
The intuition for the foldMap function is that it "annotates" each element in a
list with a Monoid newtype describing some summary operation, and then uses
mconcat to join all of the elements together and get the result of the summary.

> foldMap :: Monoid b => (a -> b) -> [a] -> b
> foldMap f xs = mconcat (map f xs)

We can use foldMap to define the "any" function a little more directly.

> any' :: [Bool] -> Bool
> any' bs = getAny (foldMap Any bs)

We'll see a more general and more convenient version of foldMap soon, but for
now, we'll only consider this basic version over lists; it's not a very
exciting pattern yet, but it becomes pretty powerful when we generalize it a
bit, so it's helpful to study the simple version first.


There are many operations that form monoids over numeric types, since many
standard arithmetic operations are associative and have a "unit" value. For
example, the Sum type represents "integers with addition", and the Product type represents "integers with multiplication".

> newtype Sum = Sum { getSum :: Int }
>   deriving (Show)
> instance Monoid Sum where
>   x <> y = Sum (getSum x + getSum y)
>   mempty = Sum 0

> newtype Product = Product { getProduct :: Int }
>   deriving (Show)
> instance Monoid Product where
>   x <> y = Product (getProduct x * getProduct y)
>   mempty = Product 1

Take a moment to think about why these are the right definitions of "mempty".
Again, remember that the meanings of these types are given by their Monoid
instances, since we're using them specifically as "annotations" for producing
monoidal summaries.

(Also, remember to try not to be scared of new terminology - this is all
simpler than you might expect on a first reading!)


*************
* PROBLEM 3 *  2 points
*************

For each data type description below, give a lawful Monoid instance that
produces the specified result when foldMap is used with the type.

Do not modify the datatype definitions.
You may modify the existing definition code for <> and mempty to add parameters
and pattern-matching cases.

a.

The function "foldMap (First . Just)" over any list should produce the first
element of the input list wrapped in Just if the list is non-empty, or Nothing
if the list is empty.

> newtype First a = First { getFirst :: Maybe a }
>   deriving (Eq, Show, Generic)

> instance Monoid (First a) where
>   x <> y = case x of
>     First Nothing -> y
>     otherwise -> x
>   mempty = First Nothing 


b.

The function "foldMap Conjoined" over a list of functions returning Bool values
should produce a function over the same input type that returns True if all of
the functions in the list return True for the given input, and False if at
least one of the functions returns False for the given input. (If the list is
empty, then all of the zero functions in the list return True.)

> newtype Conjoined a = Conjoined { getConjoined :: a -> Bool }
>   deriving Generic

> instance Monoid (Conjoined a) where
>   (Conjoined f) <> (Conjoined g) = Conjoined (\x -> (getAll (All (f x) <> All (g x))))
>   mempty = Conjoined (\y  -> True)

*****************
* END PROBLEM 3 *
*****************


A somewhat more interesting application of the monoid pattern is to join
together frequency distributions. The type "Dist a" represents a table
associating each element of type "a" with a number, which is thought of as the
"frequency" of the element, or its number of "occurrences" in the frequency
distribution. When joining frequency distributions, we add the frequencies for
each element to produce a new distribution.

We could represent a frequency distribution with a data structure implementing
a lookup table, but we'll use a more direct representation built from types
we've already defined, so that we get a Monoid instance for free.

> type Dist a = a -> Sum

With the Monoid instances we've given for function types and the Sum type, the
Dist type already has a Monoid instance.

We can define these distributions by pattern-matching, being careful to make
sure that all possible cases are covered. For example, here's a frequency distribution of Bool values where True has a frequency of 3 and False has a frequency of 5.

> exampleDist :: Dist Bool
> exampleDist True = Sum 3
> exampleDist False = Sum 5

Note that we have to wrap "3" and "5" in Sum constructors, since our Sum type
doesn't have an instance of Num. (The standard library Sum type does have an
instance of Num; this version is a little simpler for demonstration.)

Here's a second example distribution, where all even integers have a frequency
of 1 and all odd integers have a frequency of 0.

> exampleDist2 :: Dist Integer
> exampleDist2 x = if even x then Sum 1 else Sum 0

A potential downside to this representation is that it's not easy to print out,
but if "a" is an Enum type, we can straightforwardly sample some portion of the
distribution by giving lower and upper bounds.

> sample :: Enum a => a -> a -> Dist a -> [(a, Int)]
> sample low high f = map (\x -> (x, getSum (f x))) [low..high]

(Note that the Enum instance for Bool is ordered False < True.)

We haven't covered the IO type yet, so don't worry about how this printSample
function is implemented; we'll get to it soon. This can be useful when
experimenting and debugging with the Dist type in GHCi - try it out!

> printSample :: (Enum a, Show a) => a -> a -> Dist a -> IO ()
> printSample low high f =
>   forM_ (sample low high f) $ \(x,n) ->
>     putStrLn $ show x ++ ": " ++ show n


*************
* PROBLEM 4 *  3 points
*************

==== NOTE: ====
The tests for this problem depend on the answer to problem 1c; if you haven't
gotten the tests to pass for 1c yet, you probably won't be able to get the
tests to pass for this problem. Sorry about that!
===============

Replace the "undefined" in the definition below so that the "frequencies"
calculates the frequency distribution of items in the input list. For example,
the expression "frequencies [True, False, True]" should produce a distribution
where True has a frequency of 2 and False has a frequency of 1.

You may add new top-level function definitions, but do not modify any other
part of the definition of "frequencies". In particular, do not add any
additional named arguments or remove the call to "foldMap" in the definition of
"frequencies".

> frequencies :: Eq a => [a] -> Dist a
> frequencies = undefined

*****************
* END PROBLEM 4 *
*****************


Good work! Unfortunately, unlike the last assignment, there's no playable game
or anything that we get as the result of this work; this was a mostly
theoretical assignment. Next time, we'll write some actual programs that use
patterns like these!
