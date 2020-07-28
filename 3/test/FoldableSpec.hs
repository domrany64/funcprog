{-# LANGUAGE NoImplicitPrelude #-}

module FoldableSpec where

import Data.Foldable
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Map.Append as AppendMap
import Data.Map.Append (AppendMap(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intersperse)
import Data.Maybe (maybe)
import Data.Monoid (Any(..), Sum(..))
import Prelude hiding (Applicative(..), any, concat)
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Test.QuickCheck hiding (NonEmpty, elements, tabulate)
import Test.QuickCheck.Arbitrary.Generic

import Foldables

(=~=) :: Float -> Float -> Bool
x =~= y = abs (x - y) < 0.001

averageSpec ::
  forall f.
  ( Foldable f
  , Arbitrary (f Float), Show (f Float)
  ) =>
  Property
averageSpec =
  forAll arbitrary $ \(xs :: f Float) ->
    average xs =~= if length xs == 0 then 0 else (sum xs / fromIntegral (length xs))

tabulateList :: (Ord k, Monoid a) => (v -> a) -> Map k v -> [k] -> a
tabulateList t m [] = mempty
tabulateList t m (k : ks) = maybe mempty t (Map.lookup k m) <> tabulateList t m ks

tabulateSpec ::
  forall f k v a.
  ( Ord k, Foldable f, Monoid a
  , Eq a, Show k, Show v, Show a, Show (f k)
  , Function v, CoArbitrary v
  , Arbitrary k, Arbitrary v, Arbitrary a, Arbitrary (f k)
  ) =>
  Property
tabulateSpec =
  forAll arbitrary $ \(Fun _ t :: Fun v a) ->
  forAll arbitrary $ \(m :: Map k v) ->
  forAll arbitrary $ \(ks :: f k) ->
    tabulate t m ks == tabulateList t m (toList ks)

instance Arbitrary Byte where
  arbitrary = do
    x7 <- arbitrary
    x6 <- arbitrary
    x5 <- arbitrary
    x4 <- arbitrary
    x3 <- arbitrary
    x2 <- arbitrary
    x1 <- arbitrary
    x0 <- arbitrary
    return $ Bits x7 x6 x5 x4 x3 x2 x1 x0

instance CoArbitrary Byte where
instance Function Byte where

finiteByteSpec :: Property
finiteByteSpec = forAll arbitrary $ flip elem (elements :: [Byte])

intToBinary :: Int -> [Bool]
intToBinary 0 = []
intToBinary n = intToBinary (n `div` 2) ++ [n `mod` 2 == 1]

intToByte :: Int -> Byte
intToByte x =
  let
    b = intToBinary x
    [x7,x6,x5,x4,x3,x2,x1,x0] = replicate (8 - length b) False ++ b
  in
    Bits x7 x6 x5 x4 x3 x2 x1 x0

testIncSpec :: Property
testIncSpec =
  forAll arbitrary $ \(Fun _ f :: Fun Byte Byte) ->
  forAll arbitrary $ \(b :: Byte) ->
    conjoin
      [ testInc (\x -> intToByte ((byteToInt x + 1) `mod` 256))
      , if testInc f then
          byteToInt (f b) == (byteToInt b + 1) `mod` 256
        else if byteToInt (f b) /= (byteToInt b + 1) `mod` 256 then
          True
        else
          discard
      ]
      

spec :: Spec
spec = do
  describe "problem 1" $ do
    prop "average meets the specification" $
      conjoin
        [ averageSpec @Maybe
        , averageSpec @[]
        ]

  describe "problem 2" $ do
    prop "tabulate meets the specification" $
      conjoin
        [ tabulateSpec @Maybe @Int @Bool @(Sum Int)
        , tabulateSpec @[] @Bool @Char @(Maybe (Sum Int))
        ]

  describe "problem 3" $ do
    prop "Byte Finite instance includes all possible Byte values" $
      finiteByteSpec

    prop "testInc meets the specification" $
      testIncSpec

  describe "problem 4" $ do
    prop "won function meets the specification" $
      conjoin
        [ wonSpec @Cell
        , wonSpec @Int
        , wonSpec @String
        ]

instance Function Coordinate
instance CoArbitrary Coordinate

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (Board a) where
  arbitrary = genericArbitrary

instance Arbitrary Cell where
  arbitrary = genericArbitrary

extensionally ::
  forall a b.
  ( Show a, Show b, Eq b
  , Arbitrary a
  ) =>
  (b -> b -> Property) ->
  (a -> b) -> (a -> b) -> Property
extensionally p f g = forAll arbitrary $ \x -> p (f x) (g x)

extensionally2 ::
  forall a b c.
  ( Show a, Show b, Eq c
  , Arbitrary a, Arbitrary b
  ) =>
  (c -> c -> Property) ->
  (a -> b -> c) -> (a -> b -> c) -> Property
extensionally2 p f g = forAll arbitrary $ \x y -> p (f x y) (g x y)

won' :: Eq a => a -> Board a -> Bool
won' x (Board b) =
  any (all (x ==))
    [ [b (C0,C0), b (C0,C1), b (C0,C2)]
    , [b (C1,C0), b (C1,C1), b (C1,C2)]
    , [b (C2,C0), b (C2,C1), b (C2,C2)]
    , [b (C0,C0), b (C1,C0), b (C2,C0)]
    , [b (C0,C1), b (C1,C1), b (C2,C1)]
    , [b (C0,C2), b (C1,C2), b (C2,C2)]
    , [b (C0,C0), b (C1,C1), b (C2,C2)]
    , [b (C0,C2), b (C1,C1), b (C2,C0)]
    ]

wonSpec ::
  forall a.
  ( Show a, Eq a
  , Arbitrary a
  ) =>
  Property
wonSpec = extensionally2 @a (===) won won'
