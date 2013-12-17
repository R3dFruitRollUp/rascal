module Main where

import Test.Tasty
import Test.Tasty.QuickCheck     (testProperty)
import Test.Tasty.HUnit
import Test.QuickCheck

import Rascal.Utils

data Lines = Lines [String] deriving (Show)

instance Arbitrary Lines where
   arbitrary = do
      l <- listOf1 (listOf (choose ('!', '~')))
      return $ Lines l

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Invariants checked with QuickCheck" [
   testProperty "letterizeLines introduces the correct number of newlines"
      prop_letterizedLinesUnlines,
   testProperty "letterizeLines adds 4 chars per line" prop_letterizedLinesLength
   ]

prop_letterizedLinesUnlines :: Lines -> Property
prop_letterizedLinesUnlines (Lines l) =
   length l <= 25 ==>
      (length . lines . letterizeLines) l == length l

prop_letterizedLinesLength :: Lines -> Bool
prop_letterizedLinesLength (Lines l) =
   length (head l) + 4 == length (head (lines (letterizeLines l)))

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
   testCase "unescape unescapes empty" $
      unescape "" @?= "",
   testCase "unescape unescapes nothing" $
      unescape "& unescape amp nothing;" @?= "& unescape amp nothing;",
   testCase "unescape unescapes &amp;" $
      unescape "& unescape &amp; me;" @?= "& unescape & me;",
   testCase "unescape unescapes &lt; and &gt;" $
      unescape "& unescape &lt;me&gt;;" @?= "& unescape <me>;",
   testCase "no hrefs in empty" $
      hrefs "" @?= []
   ]
