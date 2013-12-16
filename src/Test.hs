module Main where

import Test.Tasty
import Test.Tasty.QuickCheck     (testProperty, QuickCheckTests(..))
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
tests = localOption (QuickCheckTests 1000) $ testGroup "QuickCheck properties" [
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
