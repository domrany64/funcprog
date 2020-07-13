{-# LANGUAGE NoImplicitPrelude #-}

module MonoidSpec (spec) where

import Prelude hiding ((<>), Monoid(..), foldMap)

import Control.Monad
import Control.Monad
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import Monoids

instance Arbitrary Any where
  arbitrary = genericArbitrary

instance Arbitrary All where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (First a) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = scale (floor . sqrt . fromIntegral) $ sized go
    where
      go n
        | n <= 0 = pure Leaf
        | otherwise = do
            i <- choose (0, n)
            oneof $
              (Node <$> arbitrary <*> go i <*> go (n-i)) :
              [ go (n-i) | i <- [1..n] ]

type EqProp a = (Show a, Eq a, Arbitrary a) => Property

type MonoidProp a = (Show a, Eq a, Monoid a, Arbitrary a) => Property

type MonoidExtProp a b =
  ( Show a, Show b, Eq b, Monoid b, Function a
  , CoArbitrary a , Arbitrary a, Arbitrary b
  ) =>
  Property

type MonoidNewtypeExtProp a b n =
  ( Show a, Show b, Eq b, Monoid n, Function a
  , CoArbitrary a , Arbitrary a, Arbitrary b
  ) =>
  ((a -> b) -> n) -> (n -> a -> b) -> Property

associativity :: forall a. MonoidProp a
associativity =
  forAll arbitrary $ \(x::a) y z ->
    x <> (y <> z) === (x <> y) <> z

leftUnit :: forall a. MonoidProp a
leftUnit =
  forAll arbitrary $ \(x::a) ->
    mempty <> x === x

rightUnit :: forall a. MonoidProp a
rightUnit =
  forAll arbitrary $ \(x::a) ->
    x <> mempty === x

extensionalAssociativity :: forall a b. MonoidExtProp a b
extensionalAssociativity =
  forAll arbitrary $ \(Fun _ f :: Fun a b) (Fun _ g) (Fun _ h) x ->
    (f <> (g <> h)) x === ((f <> g) <> h) x

extensionalLeftUnit :: forall a b. MonoidExtProp a b
extensionalLeftUnit =
  forAll arbitrary $ \(Fun _ f :: Fun a b) x ->
    (mempty <> f) x === f x

extensionalRightUnit :: forall a b. MonoidExtProp a b
extensionalRightUnit =
  forAll arbitrary $ \(Fun _ f :: Fun a b) x ->
    (f <> mempty) x === f x

extensionalNewtypeAssociativity :: forall a b n. MonoidNewtypeExtProp a b n
extensionalNewtypeAssociativity w u =
  forAll arbitrary $ \(Fun _ f :: Fun a b) (Fun _ g) (Fun _ h) x ->
    u (w f <> (w g <> w h)) x === u ((w f <> w g) <> w h) x

extensionalNewtypeLeftUnit :: forall a b n. MonoidNewtypeExtProp a b n
extensionalNewtypeLeftUnit w u =
  forAll arbitrary $ \(Fun _ f :: Fun a b) x ->
    u (mempty <> w f) x === f x

extensionalNewtypeRightUnit :: forall a b n. MonoidNewtypeExtProp a b n
extensionalNewtypeRightUnit w u =
  forAll arbitrary $ \(Fun _ f :: Fun a b) x ->
    u (w f <> mempty) x === f x

graft :: BinTree a -> BinTree a -> BinTree a
graft Leaf t = t
graft (Node x Leaf r) t = Node x t r
graft (Node x l Leaf) t = Node x l t
graft (Node x l r) t = graft l (Node x r t)

inPreorder :: [a] -> Gen (BinTree a)
inPreorder [] = pure Leaf
inPreorder (x:xs) = do
  i <- choose (0, length xs)
  let (ls,rs) = splitAt i xs
  Node x <$> inPreorder ls <*> inPreorder rs

-- preserves preorder ordering
rebalanced :: BinTree a -> Gen (BinTree a)
rebalanced = inPreorder . nodeList

-- try to change value with (roughly) given ratio of success
perturb :: Arbitrary a => Int -> Int -> a -> Gen a
perturb n d x = frequency [(n, arbitrary), (d-n, pure x)]

-- try to break preorder ordering
debalanced :: BinTree a -> Gen (BinTree a)
debalanced t =
  frequency
    [ (1, inPreorder =<< shuffle (nodeList t))
    , (99, pure t)
    ]

-- insert random nodes
inflated :: Arbitrary a => BinTree a -> Gen (BinTree a)
inflated Leaf = perturb 1 1000 Leaf
inflated (Node x l r) = do
  x' <- arbitrary
  frequency
    [ (1, pure $ Node x' (Node x l r) Leaf)
    , (1, pure $ Node x' Leaf (Node x l r))
    , (98, pure $ Node x l r)
    ]

-- delete random nodes
deflated :: BinTree a -> Gen (BinTree a)
deflated Leaf = pure Leaf
deflated (Node x l r) =
  frequency
    [ (1, graft <$> deflated l <*> deflated r)
    , (99, pure $ Node x l r)
    ]

-- try to break indexwise equality
relabeled :: Arbitrary a => BinTree a -> Gen (BinTree a)
relabeled Leaf = pure Leaf
relabeled (Node x l r) = Node <$> perturb 1 50 x <*> relabeled l <*> relabeled r

tweaked :: Arbitrary a => BinTree a -> Gen (BinTree a)
tweaked = debalanced <=< rebalanced <=< inflated <=< deflated <=< relabeled

indexwiseEquality :: forall a. EqProp a
indexwiseEquality =
  forAll arbitrary $ \(x :: BinTree a) ->
  forAll (tweaked x) $ \y ->
  forAll (choose (0, min (nodeCount x) (nodeCount y))) $ \i ->
    if x == y then
      nodeAt i x == nodeAt i y
    else if nodeAt i x /= nodeAt i y then
      x /= y
    else
      discard

reflexivity :: forall a. EqProp a
reflexivity =
  forAll arbitrary $ \(x :: BinTree a) ->
    x === x

symmetry :: forall a. EqProp a
symmetry =
  forAll arbitrary $ \(x :: BinTree a) ->
  forAll (tweaked x) $ \y ->
    if x == y then y == x else y /= x

transitivity :: forall a. EqProp a
transitivity =
  forAll arbitrary $ \(x :: BinTree a) ->
  forAll (tweaked x) $ \y ->
  forAll (tweaked y) $ \z ->
    if x == y && y == z then
      x == z
    else if x /= z then
      x /= y || y /= z
    else
      discard

firstDefinition :: forall a. EqProp a
firstDefinition =
  forAll arbitrary $ \(xs :: [a]) ->
    foldMap (First . Just) xs == case xs of
      [] -> First Nothing
      (x : _) -> First (Just x)

conjoinedDefinition :: forall a.
  ( Show a, Function a
  , CoArbitrary a, Arbitrary a
  ) =>
  Property
conjoinedDefinition =
  forAll arbitrary $ \(fs :: [Fun a Bool]) x ->
    let fs' = map applyFun fs in
      getConjoined (foldMap Conjoined fs') x == all (\f -> f x) fs'

frequenciesDefinition :: forall a. EqProp a
frequenciesDefinition =
  forAll arbitrary $ \(xs :: [a]) x ->
    getSum (frequencies xs x) == length (elemIndices x xs)

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "problem 1" $ do
    prop "a. <> is associative" $
      conjoin
        [ associativity @(Maybe ())
        , associativity @(Maybe Any)
        , associativity @(Maybe (Any, All))
        ]

    prop "a. mempty is the left unit" $
      conjoin
        [ leftUnit @(Maybe ())
        , leftUnit @(Maybe Any)
        , leftUnit @(Maybe (Any, All))
        ]

    prop "a. mempty is the right unit" $
      conjoin
        [ rightUnit @(Maybe ())
        , rightUnit @(Maybe Any)
        , rightUnit @(Maybe (Any, All))
        ]

    prop "b. <> is associative" $
      conjoin
        [ associativity @(These (Maybe Any) Any)
        , associativity @(These (Any, All) ())
        , associativity @(These () (Any, All))
        ]

    prop "b. mempty is the left unit" $
      conjoin
        [ leftUnit @(These (Maybe Any) Any)
        , leftUnit @(These (Any, All) ())
        , leftUnit @(These () (Any, All))
        ]

    prop "b. mempty is the right unit" $
      conjoin
        [ rightUnit @(These (Maybe Any) Any)
        , rightUnit @(These (Any, All) ())
        , rightUnit @(These () (Any, All))
        ]

    prop "c. <> is associative" $
      conjoin
        [ extensionalAssociativity @Int @All
        , extensionalAssociativity @(Maybe Int) @Any
        ]

    prop "c. mempty is the left unit" $
      conjoin
        [ extensionalLeftUnit @Int @All
        , extensionalLeftUnit @(Maybe Int) @Any
        ]

    prop "c. mempty is the right unit" $
      conjoin
        [ extensionalRightUnit @Int @All
        , extensionalRightUnit @(Maybe Int) @Any
        ]

  describe "problem 2" $ do
    prop "<>  is associative" $
      conjoin
        [ associativity @(BinTree ())
        , associativity @(BinTree (Maybe Bool))
        , associativity @(BinTree (Maybe Bool, Int))
        ]

    prop "== is reflexive" $
      conjoin
        [ reflexivity @(BinTree ())
        , reflexivity @(BinTree (Maybe Bool))
        , reflexivity @(BinTree (Maybe Bool, Int))
        ]

    prop "== is symmetric" $
      conjoin
        [ symmetry @(BinTree ())
        , symmetry @(BinTree (Maybe Bool))
        , symmetry @(BinTree (Maybe Bool, Int))
        ]

    prop "== is transitive" $
      conjoin
        [ transitivity @(BinTree ())
        , transitivity @(BinTree (Maybe Bool))
        , transitivity @(BinTree (Maybe Bool, Int))
        ]

    prop "== respects index-wise equality" $
      conjoin
        [ indexwiseEquality @Bool
        , indexwiseEquality @(Maybe Bool, Int)
        , indexwiseEquality @(Either Any (Maybe Int))
        ]

  describe "problem 3" $ do
    prop "a: <> is associative" $
      conjoin
        [ associativity @(First Bool)
        , associativity @(First Int)
        , associativity @(First (Maybe Bool))
        ]

    prop "a: mempty is left unit" $
      conjoin
        [ leftUnit @(First Bool)
        , leftUnit @(First Int)
        , leftUnit @(First (Maybe Bool))
        ]

    prop "a: mempty is right unit" $
      conjoin
        [ rightUnit @(First Bool)
        , rightUnit @(First Int)
        , rightUnit @(First (Maybe Bool))
        ]

    prop "a: \"foldMap (First . Just)\" produces correct result" $
      conjoin
        [ firstDefinition @Bool
        , firstDefinition @Int
        , firstDefinition @(Maybe Bool)
        ]

    prop "b: <> is associative" $
      conjoin
        [ extensionalNewtypeAssociativity @Bool Conjoined getConjoined
        , extensionalNewtypeAssociativity @Int Conjoined getConjoined
        , extensionalNewtypeAssociativity @(Maybe Bool) Conjoined getConjoined
        ]

    prop "b: mempty is left unit" $
      conjoin
        [ extensionalNewtypeLeftUnit @Bool Conjoined getConjoined
        , extensionalNewtypeLeftUnit @Int Conjoined getConjoined
        , extensionalNewtypeLeftUnit @(Maybe Bool) Conjoined getConjoined
        ]

    prop "b: mempty is right unit" $
      conjoin
        [ extensionalNewtypeRightUnit @Bool Conjoined getConjoined
        , extensionalNewtypeRightUnit @Int Conjoined getConjoined
        , extensionalNewtypeRightUnit @(Maybe Bool) Conjoined getConjoined
        ]

    prop "b: \"foldMap Conjoined\" produces correct result" $
      conjoin
        [ conjoinedDefinition @Bool
        , conjoinedDefinition @Int
        , conjoinedDefinition @(Maybe Bool)
        ]

  describe "problem 4" $
    prop "\"frequencies\" produces correct result" $
      conjoin
        [ frequenciesDefinition @Bool
        , frequenciesDefinition @Int
        , frequenciesDefinition @(Maybe Bool)
        ]
