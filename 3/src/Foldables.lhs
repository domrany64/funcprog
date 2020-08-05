> {-# LANGUAGE NoImplicitPrelude #-}

This assignment has you work with the Foldable, Functor, Applicative, and Monad
typeclasses, along with the Monoid typeclass from the previous assignment. The
main goal is to get hands-on experience with Haskell's standard higher-kinded
typeclasses and to demonstrate how they can help us avoid a lot of boilerplate
code when defining standard functions for custom "container types".

There are many different sensible formal definitions of the word "container"
when describing parametric types; we would generally expect types like List and
Maybe to be containers in any interpretation, while e.g. certain function types
can be seen as "containers" in some senses and not in others.

Foldable, Functor, and Applicative are all very general typeclasses that have
many instances that might be surprising from a programming perspective, but
we'll be using them in this assignment specifically to classify different
notions of container types. At an intuitive level, if we have a parametric
container type "F :: * -> *":

  - F is foldable if we can "concatenate" a container of type "F a" to a single
    "a" whenever "a" is a monoid; equivalently, F is a foldable if we can
    convert a container of type "F a" to a list of "a" elements that contains
    all of the elements from the container. (The Foldable typeclass and the
    Listable typeclass from lecture are logically equivalent, but the Foldable
    formulation of the concept usually has better performance characteristics.)
  - F is a functor if we can "map" a *single function* over all elements of
    a container of type "F a", obtaining a new container as output.
  - F is an applicative if we can create a "singleton" container given one
    element and we can "zip" multiple containers together with
    multiple-argument functions over elements.

To be clear, not all Functor/Foldable/Applicative instances follow these
intuitions, but they do apply to instances for most types that we would think
of as "container types".

Most of the super common container types (List, Maybe, etc.) implement all
three of Functor/Foldable/Applicative, but some interesting types are
containers in one sense but not another. By definition every Applicative is
also a Functor, so the "feature matrix" that we get from combining these three
typeclasses doesn't include all possible combinations: in total, we get six
sensible combinations of typeclasses, each classifying a different subset of
the functionality we sometimes associate with "containers".

  - no instances - can't concatenate, map, or zip
  - Foldable T - can concatenate but not map or zip
  - Functor T - can map but not concatenate or zip
  - (Foldable T, Functor T) - can map and concatenate but not zip
  - (Functor T, Applicative T) - can map and zip but not concatenate
  - (Foldable T, Functor T, Applicative T) - can map, zip, and concatenate

There are benefits and drawbacks to having this fine-grained hierarchy instead
of a single "Container" typeclass that captures all of this functionality. In
recent versions of GHC, the ConstraintKinds extension allows us to write a type
synonym for a constraint, so we could define this type and use it seamlessly: a
type signature like "foo :: Container f => ..." means that the "f" type
variable has instances for all three of these typeclasses.

  type Container f = (Foldable f, Functor f, Applicative f)

But while we will generally be using concrete types that are instances of
this Container type, many functions we write will only require concatenation
*or* mapping, so these functions can be written with more lenient constraints.
This is in keeping with the "principle of least privilege", and in the spirit
of encapsulation via interfaces in object-oriented programming: the less a
function assumes about the concrete types of its input, the harder it is to
write a type-correct but logically incorrect implementation of the function.

Also, there are many more operations we might associate with a container -
should it support removal of elements, sorting, random access by index, ...?
Each additional operation adds functionality to the interface but restricts the
set of types it applies to. This is a problem for a library author trying to
provide a meaningful abstraction that will be valuable in a useful variety of
client code! Different languages and paradigms propose different solutions and
workarounds for this problem; the standard solution in the Haskell Prelude is
to define many small typeclasses that each capture a single concept, letting
the user combine them when needed.

> module Foldables where

> import Prelude hiding (Applicative(..), any, concat)
> import Data.Foldable
> import Data.Semigroup

We'll be using some types from the "containers" package, as in the last
assignment. We'll also import the "union map" type instead of defining our own:
the AppendMap type from the "appendmap" package provides the same functionality
as the Union type from the last assignment.

> import qualified Data.Map as Map
> import Data.Map (Map)
> import Data.Map.Append (AppendMap(..))
> import Data.List (intersperse)
> import Data.Maybe (maybe)
> import Data.Monoid (Any(..), Sum(..))
> import GHC.Generics (Generic)

Foldable doesn't actually have any laws of its own, which is uncommon for
higher-kinded typeclasses. (There's a Prelude class called Traversable that
generalizes Foldable and does have sensible laws, but we won't cover it here.)

  class Foldable (f :: * -> *) where
    foldMap :: forall a b. Monoid b => (a -> b) -> f a -> b

We'll be using the built-in Foldable typeclass in this file, so these
definitions of basic Foldable concepts will be in comments to avoid conflicting
with the built-in definitions.

Intuitively, the foldMap function "reduces" a container of values by
transforming all of the values into some monoid type and then "concatenating"
all of the values together.

There is one law that relates Foldable and Functor, for any type that has
instances for both:

  foldMap (g . f) == foldMap g . fmap f

This is similar to the functor composition law: intuitively, it says that the
"map" part of the "foldMap" operation must agree with the "fmap" operation from
the Functor instance.

The prototypical foldable type is the list type. Note that "fmap" and "map"
mean the same thing when applied to lists; you can use whichever you want when
working with lists, but the given code in this file will use "fmap" to
emphasize that we're working with the list type as a functor.

  instance Foldable [] where
    foldMap :: Monoid b => (a -> b) -> [a] -> b
    foldMap f = mconcat . fmap f

One standard function that we can write over an arbitrary Foldable is the
function that converts a container to a list: we convert each element to a
singleton list, and then concatenate all the lists together.

  toList :: Foldable f => f a -> [a]
  toList = foldMap (\x -> [x])

As mentioned in the introduction, Foldable is actually logically equivalent to
the Listable type from the lecture slides on higher-kinded types, so foldMap
can be seen as having equivalent "power" as toList: we can define either one in
terms of the other.

  foldMap :: (Listable f, Monoid b) => (a -> b) -> f a -> b
  foldMap f = mconcat . fmap f . toList

This implementation of foldMap is often less than optimally efficient, though -
even if all of the space overhead of converting to a list is optimized away, it
still commits to processing elements linearly (one at a time), while some
Foldable instances may have implementations of foldMap in sub-linear time.


The Foldable instance for Maybe relies on the intuition that a value of type
"Maybe a" contains either zero or one values of type "a". If there are zero
values, we return mempty (from the Monoid instance for "b"); if there's one
value, we just apply our transforming function to that value.

This is a very terse definition, but it illustrates that the "maybe" function
acts as a sort of "folding" operation for the Maybe type. Check the type of
"maybe" in GHCi and test it out on some sample inputs if you're not clear on
how it works - we haven't introduced it in class, but by this point I think
you're up to the challenge of deciphering this code!

  instance Foldable Maybe where
    foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap = maybe mempty


Actually, in modern versions of GHC, we can derive Foldable instances
automatically! This requires the DeriveFoldable language extension, which is
enabled in the .cabal file for this project. For example, here's a collection
type where each collection value contains either two or three elements, which
we might use to implement the branching in a data structure like a 2-3 tree.

> data TwoThree a = Two a a | Three a a a
>   deriving (Foldable)


*************
* PROBLEM 1 *  2 points
*************

Replace the "undefined" in the definition of "average" below so that the
function produces the average (mean) of all of the elements in the input
collection. Do not modify any other code.

> type RunningTotal = (Sum Float, Sum Float)

> getAverage :: RunningTotal -> Float
> getAverage (_, 0) = 0
> getAverage (total, count) = getSum total / getSum count

> average :: Foldable f => f Float -> Float
> average = getAverage . foldMap undefined

*****************
* END PROBLEM 1 *
*****************


One last interesting container type that we haven't seen yet is the Map type,
which this file imports from the module Data.Map from the "containers" package.
This is a standard "dictionary" or "associative array" type, implemented
similarly to a hashmap under the hood.

Because the module Data.Map is imported using "import qualified" syntax, if we
want to use a function from Data.Map, we have to prefix it with the identifier
"Map" and a period; this is like using namespaces in C++, or qualified imports
in many other languages.

Don't get confused between the type "Map" and the function "map" - they're
very different things!

The type "Map k v" is the type of dictionaries that associate keys of type "k"
with values of type "v". The standard way to construct a Map value is with the
Map.fromList function:

  Map.fromList :: Ord k => [(k,v)] -> Map k v

Many Map functions require that the key type has an instance of Ord, since the
Map is internally structured like an ordered tree for efficiency; this is just
an implementation detail and can usually be ignored.

Note that the Show instance for the Map function prints Map values in terms of
Map.fromList, which is a little strange. It does this to avoid showing you
unnecessary details about the internal structure of the Map.

Like many collection types, the Map type is Foldable - specifically, the type
"Map k" for any given type "k" has a Foldable instance. We can't say that "Map"
itself is Foldable, because it has the wrong kind, * -> * -> *. This means that
our collection type of kind * -> * is really "Map k", with a given fixed key
type, so the Foldable instance operates only over the collection of values in
the map, ignoring the keys.

There are many Map-specific functions for working with Map values, but in this
assignment we're mostly going to work with Map values by using the Foldable
instance. Other than that, here are two more library functions that might be
helpful when debugging or writing code that operates on Map values:

  Map.lookup :: Ord k => k -> Map k a -> Maybe a
  Map.singleton :: k -> a -> Map k a

Try them out in GHCi!


There are many useful functions we can define over an arbitrary Foldable type.
Since Foldable is equivalent to Listable, this is effectively the same set of
functions that we can define by converting a container to a list and doing some
operation over the list, but foldMap lets us give more direct definitions.

  sum :: (Foldable f, Num a) => f a -> a
  sum = getSum . foldMap Sum

  any :: Foldable f => (a -> Bool) -> f a -> Bool
  any f = getAny . foldMap (Any . f)

  elem :: (Foldable f, Eq a) => a -> f a -> Bool
  elem x = any (x ==)

We also get a generalized version of "mconcat" that works over any Foldable.

  fold :: (Foldable f, Monoid a) => f a -> a
  fold = foldMap id

An interesting special case of the "fold" function is the list concatenation
function: a container of lists can be reduced to a single list, using the
Monoid instance for the list type.

  concat :: Foldable f => f [a] -> [a] -- concat @[] :: [[a]] -> [a]
  concat = fold

Note that we can't have a function with type "Foldable f => [f a] -> f [a]",
because the Foldable interface doesn't give us the ability to combine multiple
containers together into a single container (or to create a new container).
It can be a good exercise to try to write this function and see why you can't!


*************
* PROBLEM 2 *  3 points
*************

Replace the "undefined" in the definition of "tabulate" below so that the
function produces the monoidal summary of the values in the given Map that
correspond to the keys in the given Foldable collection.

In other words, for each element "x" in the Foldable input collection that is a
valid key in the map "m", you should look up the value associated with the key
"x" in the map "m" and apply the function "t" to the result; the foldMap
function will then join all those results together as a monoidal summary.
Any keys in the Foldable input collection that are not valid keys in the map
"m" should be ignored.

> tabulate :: (Ord k, Foldable f, Monoid a) => (v -> a) -> Map k v -> f k -> a
> tabulate t m = foldMap undefined


Here are a couple examples to illustrate some uses of "tabulate", which should
produce the expected outputs if you have the correct definition.

We can use "tabulate" along with the Sum monoid to produce the total cost of a
list of selections on a menu of prices; in the example below, the item "X" has
a cost of 1, "Y" has a cost of 2, and "Z" has a cost of 3, and the value of
"totalCost" should be 1+2+1+3+2=9.

> menu :: Map String Int
> menu = Map.fromList [("X",1), ("Y",2), ("Z",3)]

> totalCost :: Int
> totalCost = getSum (tabulate Sum menu ["X", "Y", "X", "Z", "Y"])

Since the list type is already a monoid, we can use "tabulate id" with a Map
containing list values; this produces the concatenation of the lists associated
with each of the given keys. In the example below, the "parts" dictionary
represents a list of parts needed to construct various items: the item "X"
requires one "A" part and one "B" part to construct. The "totalParts"
definition takes in a TwoThree value of keys to process, and should produce the
list ["A","B","A","B","A","C","D"].

> parts :: Map String [String]
> parts =
>   Map.fromList
>     [ ("X", ["A", "B"])
>     , ("Y", ["A", "C", "D"])
>     , ("Z", ["B"])
>     ]

> totalParts :: [String]
> totalParts = tabulate id parts (Three "X" "X" "Y")

*****************
* END PROBLEM 2 *
*****************


One last Foldable function that will be useful later is this elemCounts
function, which returns a Map indicating how many occurrences of each value are
in the container. The AppendMap type used here is a newtype wrapper for the Map
type that gives it a useful Monoid instance: the (<>) operator over AppendMap
values takes the union of two maps and joins the values of any duplicate keys
with the (<>) operator from the Monoid instance for the value type.

> elemCounts :: (Foldable f, Ord a) => f a -> Map a Int
> elemCounts = fmap getSum . unAppendMap . foldMap s
>   where
>     s :: a -> AppendMap a (Sum Int)
>     s x = AppendMap (Map.singleton x (Sum 1))


We'll take a quick detour here to talk about finite types. The motivation is
that function types with a finite input domain can be seen as containers: for
example, a function "f :: Bool -> A" can be seen as a pair of values of type
"A", specifically "f True :: A" and "f False :: A". We saw this in the first
assignment, where we defined a tic-tac-toe board as a function from the
nine-element Index type to the Cell type; we'll revisit this example later in
this assignment.

There are many equivalent ways to encode "finiteness". This Finite typeclass
is one way to capture the notion: it says that a type is finite if we can
create a list that contains every element of the type at some determinable
index.

This definition carries a lot of theoretical weight, but keep in mind that
programmatically it just says that a Finite type is one where we have a finite
list containing all of the elements of the type.

> class Finite (a :: *) where
>   -- exists n. length elements == n
>   -- forall x. elem x elements == True
>   elements :: [a]

The first law is just saying that "elements" must be a finite list; we haven't
talked much about infinite lists yet, but they exist in Haskell, so we have to
specify that this is not an infinite list. The second law says that every value
of type "a" can be found somewhere in the list.

As mentioned above, Bool is a finite type: it has exactly two values. It's easy
to see that this definition satisfies the Finite laws.

> instance Finite Bool where
>   elements = [True, False]

The type "Either a b" is finite when both "a" and "b" are finite.

> instance (Finite a, Finite b) => Finite (Either a b) where
>   elements = fmap Left elements ++ fmap Right elements

Similarly, the type "(a,b)" is finite when both "a" and "b" are finite.

> instance (Finite a, Finite b) => Finite (a, b) where
>   elements = allPairs elements elements


Any function type with a finite input type is Foldable: we retrieve the list of
all possible inputs, apply the function to all of those inputs, and join all of
those results together monoidally.

> instance Finite a => Foldable ((->) a) where
>   foldMap :: Monoid c => (b -> c) -> (a -> b) -> c
>   foldMap f g = foldMap (f . g) elements

This lets us treat any function type with a finite input type as a collection,
specifically for the purpose of producing a monoidal summary of its contents.


*************
* PROBLEM 3 *  2 points
*************

A function with a finite input type is particularly easy to test: a test suite
can simply call the function with every possible input and check that it
produces the correct result in every case. This is a fundamental piece of the
testing paradigm known as "model checking", often used in the verification of
digital circuits.

Consider an eight-bit byte type, represented by a container of eight Bool
values.

> data Byte = Bits Bool Bool Bool Bool Bool Bool Bool Bool
>   deriving (Generic, Eq, Show)

The Byte type can be interpreted as an unsigned binary integer type: for
example, the value "Bits True False True True False False False True"
represents the binary byte "10110001", which is the unsigned binary
representation of the decimal number 177.

Here's a function to convert a Byte to an Int, which might be helpful in GHCi.

> byteToInt :: Byte -> Int
> byteToInt (Bits x7 x6 x5 x4 x3 x2 x1 x0) =
>   sum [bitToInt x * 2^n | (x,n) <- zip [x0,x1,x2,x3,x4,x5,x6,x7] [0..7]]
>   where
>     bitToInt :: Bool -> Int
>     bitToInt False = 0
>     bitToInt True = 1

A function over Byte values can be interpreted as an 8-bit gate in a logical
circuit; for example, the "div2" function below halves an unsigned integer
(rounding down) by shifting all of the bits to the right one position and
dropping the least-significant bit.

> -- spec: forall (b :: Byte), byteToInt (div2 b) == div (byteToInt b) 2
> div2 :: Byte -> Byte
> div2 (Bits x7 x6 x5 x4 x3 x2 x1 _) = Bits False x7 x6 x5 x4 x3 x2 x1

a.

Give a Finite instance for the Byte type.
Do not use the True and False constructors explicitly in your definition.

(Hint: use the list monad or a list comprehension.)

> instance Finite Byte where
>   elements = undefined


b.

Replace the "undefined" in the testInc definition to produce a function that
takes in a value of type "Byte -> Byte" and detects whether it represents an
8-bit gate that adds 1 to its argument (overflowing to 0 if the maximum input
is passed in).

Formally:
  if "testInc f" returns True,
  then for all possible choices of Byte value "b",
  "byteToInt (f b) == ((byteToInt b + 1) `mod` 256)" should return True;

  if "testInc f" returns False,
  then there should exist at least one choice of Byte value "b"
  for which "byteToInt (f b) == ((byteToInt b + 1) `mod` 256)" returns False.

Don't overthink this; before you start writing code, try to come up with a
reasonable plan that won't take too much work to code up.

> testInc :: (Byte -> Byte) -> Bool
> testInc f = undefined


*****************
* END PROBLEM 3 *
*****************


Another fundamental higher-kinded typeclass in Haskell is the Applicative
class. With container types, the intuition for Applicative is that we have a
sort of "cross-apply" operation: the (<*>) operator takes a container of
functions and applies those functions to a container of values, producing a new
container as output. The "pure" function creates a container given a single
"seed" element: this might be a container that contains exactly one value, but
it might also be a container with several copies of the one value, or with some
extra structure.

This is a very general abstraction, but we'll be using it in a fairly limited
way in this assignment, so we'll avoid talking about some of the Applicative
laws for now; the ones given here are a subset of the applicative laws that the
Prelude lists in the standard Applicative definition.

> class Functor f => Applicative (f :: * -> *) where
>   pure :: forall a. a -> f a

>   -- pure f <*> xs == fmap f xs
>   -- pure f <*> pure x = pure (f x)
>   infixl 4 <*>
>   (<*>) :: forall a b. f (a -> b) -> f a -> f b

The "<*>" operator is sometimes pronounced "ap" (as in "apply"), and it has
this name in the Prelude, but we'll see a better way to pronounce it in
some expressions below.

The first law says that applying a container that contains just one function to
a container of elements has the same result as using "fmap" to apply the
function to each element in the container.

The second law says that applying a container that contains just one function
to a container that contains just one element results in a container that
contains just one result, that of the function applied to the element.

The Applicative class is definitely best understood through examples. The list
type can actually be given multiple different lawful Applicative instances, but
this is the standard one defined in the Prelude: "pure" creates a
single-element list, and "fs <*> xs" applies each function in "fs" to each
value in "xs" and collects the results.

> instance Applicative [] where
>   pure :: a -> [a]
>   pure x = [x]
> 
>   (<*>) :: [a -> b] -> [a] -> [b]
>   fs <*> xs = [f x | f <- fs, x <- xs]

One of the primary features of the Applicative typeclass, which is probably not
obvious at first, is that it allows us to "map" multiple-argument functions
over multiple containers. We can write a standard function called liftA2 that
acts as a two-argument version of fmap:

> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> liftA2 f x y = pure f <*> x <*> y

It's interesting to explore why we can't define liftA2 with just a Functor
constraint, but we'll put that aside for now. This is a subtle definition, and
it's worth spending a minute to try to make sense of it. The (<*>) operator is
left-associative, so this is the same as "(pure f <*> x) <*> y". The types work
out as follows (check for yourself):

  f :: a -> b -> c
  x :: f a
  y :: f b

  pure f             :: f (a -> b -> c)
  pure f <*> x       :: f (b -> c)
  pure f <*> x <*> y :: f c

Similarly, we can write functions liftA3, liftA4, etc. for arbitrary
Applicative types. In general, the expression "pure f <*> x <*> y <*> ..." can
be read as "mapping" the multiple-argument function "f" over multiple container
inputs. This pattern is the most common use of <*>, so we might pronounce the
definition of liftA2 as "map f over the containers x and y", keeping in mind
that "mapping" over multiple arguments involves the Applicative typeclass.

A canonical example of liftA2 over lists is the function that creates a list of
all ordered pairs of elements from two input lists.

> allPairs :: [a] -> [b] -> [(a,b)]
> allPairs = liftA2 (,)

If you trace out the steps of computation involved in an application of
allPairs, it creates a list of functions of the form "\y -> (x,y)" for each
element "x" in the first list, and applies each function in that list to each
element of the second list to produce a list of pairs.

Another standard Applicative type is the Maybe type. There is only one lawful
Applicative instance for Maybe: "pure" creates a non-empty Maybe value (Just)
from a single element, and <*> combines a function with a value if both exist.

> instance Applicative Maybe where
>   pure :: a -> Maybe a
>   pure = Just
> 
>   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
>   Just f <*> Just x = Just (f x)
>   _ <*> _ = Nothing


Now let's bring all these concepts together! We'll use the same example from
the first assignment: checking whether a given player has won a game of
tic-tac-toe, in a way that easily generalizes to boards of other sizes and
shapes.

First, let's reintroduce the Index and Board types from the first assignment.

> data Coordinate = C0 | C1 | C2
>   deriving (Generic, Eq, Ord, Show)

This type is clearly finite, with three distinct values.

> instance Finite Coordinate where
>   elements = [C0, C1, C2]

An Index is a pair of coordinates, and has a Finite instance given by the
general instance for pairs above.

> type Index = (Coordinate, Coordinate)

A Board is a container of nine elements. The GeneralizedNewtypeDeriving GHC
extension allows us to automatically derive instances for newtypes whenever the
"wrapped" type has those instances: here we use this to derive several useful
bits of functionality for our Board type, since the type "(->) Index" already
has instances for all of these typeclasses.

> newtype Board a = Board { cell :: Index -> a }
>   deriving (Generic, Functor, Foldable, Semigroup, Monoid)

(You can ignore the Generic instance as usual, I'm just using it for testing.
The Semigroup instance is just a requirement to derive the Monoid instance.)

To be specific, these are the constraints on the generated instances:

  instance Functor Board where ...
  instance Foldable Board where ...
  instance Monoid a => Monoid (Board a) where ...

The Monoid instance combines two boards elementwise:

  ( x1 x2 x3 )    ( y1 y2 y3 )   ( (x1 <> y1) (x2 <> y2) (x3 <> y3) )
  ( x4 x5 x6 ) <> ( y4 y5 y6 ) = ( (x4 <> y4) (x5 <> y5) (x6 <> y6) )
  ( x7 x8 x9 )    ( y7 y8 y9 )   ( (x7 <> y7) (x8 <> y8) (x9 <> y9) )

The Monoid instance defines "mempty" as a board full of "mempty" values.

Here's the Show instance from last time, which gives a 2D display of the board.

> instance Show a => Show (Board a) where
>   show b = unlines (fmap showRow rows)
>     where
>       rows = [[(cx,cy) | cx <- elements] | cy <- elements]
>       showRow = concat . intersperse " " . fmap (show . cell b)

Here's a Cell type and an example board to play around with in GHCi.

> data Cell = X | O | E -- E for Empty
>   deriving (Generic, Eq)

> instance Show Cell where
>   show X = "X"
>   show O = "O"
>   show E = "."

> example :: Board Cell
> example = Board grid
>   where
>     grid :: Index -> Cell
>     grid (C0,C0) = X; grid (C1,C0) = O; grid (C2,C0) = E
>     grid (C0,C1) = X; grid (C1,C1) = X; grid (C2,C1) = E
>     grid (C0,C2) = O; grid (C1,C2) = E; grid (C2,C2) = E


The Board type is an applicative.

> instance Applicative Board where
>   pure :: a -> Board a
>   pure x = Board (\i -> x)

>   (<*>) :: Board (a -> b) -> Board a -> Board b
>   fb <*> xb = Board (\i -> cell fb i (cell xb i))

(Incidentally, these are the K and S combinators from combinatory logic.)

The "pure" function creates a Board where every element has the same value.

           ( x x x )
  pure x = ( x x x )
           ( x x x )

The <*> function is elementwise application of a board of functions to a board
of elements.

  ( f1 f2 f3 )     ( x1 x2 x3 )   ( (f1 x1) (f2 x2) (f3 x3) )
  ( f4 f5 f6 ) <*> ( x4 x5 x6 ) = ( (f4 x4) (f5 x5) (f6 x6) )
  ( f7 f8 f9 )     ( x7 x8 x9 )   ( (f7 x7) (f8 x8) (f9 x9) )

A consequence of these two behaviors is that the "liftA2" function is
elementwise application of a single two-argument function to two boards of
elements.

           ( x1 x2 x3 ) ( y1 y2 y3 )   ( (f x1 y1) (f x2 y2) (f x3 y3) )
  liftA2 f ( x4 x5 x6 ) ( y4 y5 y6 ) = ( (f x4 y4) (f x5 y5) (f x6 y6) )
           ( x7 x8 x9 ) ( y7 y8 y9 )   ( (f x7 y7) (f x8 y8) (f x9 y9) )

Now that we have all these high-level operations over the Board type, let's put
them to use! The code below revisits the problem of checking whether some
player has won a game of tic-tac-toe, given the current state of the board.

We'll take a more abstract approach this time, using a modified tic-tac-toe
ruleset that works out to be equivalent to the standard ruleset. Conceptually,
we'll say that every time a player makes a mark on the board, they collect one
"token" for each different way that mark could lead to a win; for example,
placing a mark in the upper-left corner yields one "row 0" token, one "column
0" token, and one "diagonal upper-left to lower-right" token. A player has won
when they've collected three of the same token.

> data Token
>   = Row Coordinate
>   | Column Coordinate
>   | Diag1 -- upper-left to lower-right
>   | Diag2 -- lower-left to upper-right
>   deriving (Eq, Ord, Show)

Let's look at an example with a concrete board state:

  X O X
  X . O   -- "." is an empty space (E)
  X O O

Here are the tokens that each player has collected in this game so far:

  X: [ Row C0, Column C0, Diag1 -- upper-left
     , Row C0, Column C2, Diag2 -- upper-right
     , Row C1, Column C0        -- center-left
     , Row C2, Column C0        -- lower-left
     ]

  O: [ Row C0, Column C1        -- upper-center
     , Row C1, Column C2        -- center-right
     , Row C2, Column C1        -- lower-center
     , Row C2, Column C2, Diag2 -- lower-right
     ]

Player X has won this game because they've collected three "Column C0" tokens.
Player O has not won because they don't have three of any particular token.

We need a procedure to generate these token lists from a given board state.
There are many ways to approach this; we'll start by building a board that has
all possible tokens on it, and then filter it down to only the tokens that
belong to a particular player.

This "rowBoard" definition is a board with all of the Row tokens placed.
Check it out in GHCi - it actually prints nicely!

> rowBoard :: Board [Token]
> rowBoard = Board (\(cx,cy) -> [Row cx])

Similarly, the columnBoard and diagBoard1/2 functions represent the placements
of the other three kinds of tokens.

> columnBoard :: Board [Token]
> columnBoard = Board (\(cx,cy) -> [Column cy])

> diagBoard1 :: Board [Token]
> diagBoard1 = Board (\(cx,cy) -> if cx == cy then [Diag1] else [])

> diagBoard2 :: Board [Token]
> diagBoard2 = Board (\(cx,cy) -> if cx == mirror cy then [Diag2] else [])
>   where
>     mirror :: Coordinate -> Coordinate
>     mirror C0 = C2
>     mirror C1 = C1
>     mirror C2 = C0

We can combine these boards with the Monoid instance for Board: the type
"Board [Token]" is a monoid whose <> action is cellwise list concatenation.

This is the board with all of the tokens placed.

> tokenBoard :: Board [Token]
> tokenBoard = mconcat [rowBoard, columnBoard, diagBoard1, diagBoard2]

Almost there! Now we need a way to filter it down to only the tokens that one
given player owns. We'll do this by taking a game board and the token board and
"zipping" them together with an operation that keeps only the stacks of tokens
in cells corresponding to marks from the given player on the game board.

The "playerFilter" function is going to be the "zipping" function that we
combine the two boards with; we'll use it partially applied with one argument,
so remember this type can also be written "a -> (a -> [Token] -> [Token])".

> playerFilter :: Eq a => a -> a -> [Token] -> [Token]
> playerFilter x x' ys = if x == x' then ys else []

For a given "x", the function "playerFilter x :: a -> [Token] -> [Token]" can
be seen as a function that conditionally empties a list when its first argument
is not "x". Conceptually, this represents the action of removing from the board
all of the stacks of tokens that don't belong to "x" from the board.

Now, if we have a board of pieces of type "a", we can combine it with
"tokenBoard" using "liftA2 (playerFilter x)" to obtain the board with only the
tokens that belong to "x".

> playerBoard :: Eq a => a -> Board a -> Board [Token]
> playerBoard x b = liftA2 (playerFilter x) b tokenBoard


*************
* PROBLEM 4 * 3 points
*************

Replace the "undefined" in the function below so that "won" returns a Bool
indicating whether the given player has won on the given board. You may use
other functions that you define in this file, but you may not change any of the
definition of "won" except for the "undefined" part.

Do not use any of the Coordinate, Token, Board, or Cell constructors
explicitly, in this definition or the definitions of any additional functions
that you write. Don't use "toList" or any functions that use it either, and
don't add or modify any imports to this file.

This problem can be solved in several different ways by combining a small
number of the Foldable functions introduced in this assignment. (The solution I
have in the solution key is a composition of three functions.) This is a common
pattern in Haskell: many problems involving containers can be solved concisely
with the standard Prelude Foldable functions!

Here's a tip to get you started: the "elemCounts" function might be useful, and
remember that the Map type has a Foldable instance. Also, while you shouldn't
use "toList" in your answer, it might be useful for experimenting in GHCi!

> won :: Eq a => a -> Board a -> Bool
> won x = undefined

*****************
* END PROBLEM 4 *
*****************
