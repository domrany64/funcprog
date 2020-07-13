module TicTacToeSpec (spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TicTacToe

instance Arbitrary Coordinate where
  arbitrary = elements [C0, C1, C2]

instance CoArbitrary Coordinate

instance Arbitrary Cell where
  arbitrary = oneof [return Empty, Mark <$> arbitrary]

instance Arbitrary Board where
  arbitrary = Board <$> arbitrary

instance Arbitrary Player where
  arbitrary = elements [X, O]

wonSpec :: Board -> Player -> Bool
wonSpec b x =
  or
    [ nub [cell b (C0,C0), cell b (C1,C1), cell b (C2,C2)] == [Mark x]
    , nub [cell b (C0,C2), cell b (C1,C1), cell b (C2,C0)] == [Mark x]
    , nub [cell b (C0,C0), cell b (C0,C1), cell b (C0,C2)] == [Mark x]
    , nub [cell b (C1,C0), cell b (C1,C1), cell b (C1,C2)] == [Mark x]
    , nub [cell b (C2,C0), cell b (C2,C1), cell b (C2,C2)] == [Mark x]
    , nub [cell b (C0,C0), cell b (C1,C0), cell b (C2,C0)] == [Mark x]
    , nub [cell b (C0,C1), cell b (C1,C1), cell b (C2,C1)] == [Mark x]
    , nub [cell b (C0,C2), cell b (C1,C2), cell b (C2,C2)] == [Mark x]
    ]

emptyIxsSpec :: Board -> [Index]
emptyIxsSpec b =
  concat
    [ if emptyAt b (C0,C0) then [(C0,C0)] else []
    , if emptyAt b (C1,C0) then [(C1,C0)] else []
    , if emptyAt b (C2,C0) then [(C2,C0)] else []
    , if emptyAt b (C0,C1) then [(C0,C1)] else []
    , if emptyAt b (C1,C1) then [(C1,C1)] else []
    , if emptyAt b (C2,C1) then [(C2,C1)] else []
    , if emptyAt b (C0,C2) then [(C0,C2)] else []
    , if emptyAt b (C1,C2) then [(C1,C2)] else []
    , if emptyAt b (C2,C2) then [(C2,C2)] else []
    ]

aiSpec :: Board -> Property
aiSpec b =
  if inProgress b then
    forAll (arbitrary `suchThat` emptyAt b) $ \i ->
      let b' = write i X b in
        if inProgress b' then
          aiSpec (aiAct b' O)
        else
          property $ not $ won b' X
  else
    property $ not $ won b X

spec :: Spec
spec = do
  prop "rowIxs has the right contents" $ \cy ->
    let is = rowIxs cy in
      length is == 3 && all (flip elem is) [(C0,cy), (C1,cy), (C2,cy)]

  prop "columnIxs has the right contents" $ \cx ->
    let is = columnIxs cx in
      length is == 3 && all (flip elem is) [(cx,C0), (cx,C1), (cx,C2)]

  prop "downDiagIxs has the right contents" $
    let is = downDiagIxs in
      length is == 3 && all (flip elem is) [(C0,C0), (C1,C1), (C2,C2)]

  prop "upDiagIxs has the right contents" $
    let is = upDiagIxs in
      length is == 3 && all (flip elem is) [(C0,C2), (C1,C1), (C2,C0)]

  prop "emptyIxs has the correct output" $ \b ->
    sort (emptyIxsSpec b) == sort (emptyIxs b)

  prop "won works correctly with two players" $ \x b ->
    wonSpec b x == won b x

  prop "aiMove never loses a two-player game" $
    aiSpec emptyBoard
