module Main where

import Data.List                 (elemIndices, findIndices, union, isPrefixOf)
import Test.Tasty
import Test.Tasty.QuickCheck     (testProperty)
import Test.Tasty.HUnit
import Test.QuickCheck
import Data.Map                  (assocs)
import Control.Applicative       ((<$>))

import Rascal.Utils
import Rascal.Conf

data Lines = Lines [String] deriving (Show)

instance Arbitrary Lines where
   arbitrary =
      Lines <$> listOf1 (listOf (choose ('!', '~')))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Invariants checked with QuickCheck"
   [ testProperty "letterizeLines introduces the correct number of newlines"
      prop_letterizedLinesUnlines
   , testProperty "letterizeLines adds 4 chars per line"
      prop_letterizedLinesLength
   , testProperty "cleanUp only add lines"
      prop_cleanUpLines
   , testProperty "cleanUp only replaces spaces by newlines"
      prop_cleanUpSpacesToLines
   , testProperty "indentLines keeps lines under width"
      prop_indentStringHasWidth
   , testProperty "indentLines properly adds prefix"
      prop_indentStringInsertsPrefix
   ]

prop_letterizedLinesUnlines :: Lines -> Property
prop_letterizedLinesUnlines (Lines l) =
   length l <= 25 ==>
      (length . lines . letterizeLines) l == length l

prop_letterizedLinesLength :: Lines -> Bool
prop_letterizedLinesLength (Lines l) =
   length (head l) + 4 == length (head (lines (letterizeLines l)))

data WordList = WordList
   { wrds :: String
   , len :: Int
   } deriving (Show)

instance Arbitrary WordList where
   arbitrary = do
      Lines w <- arbitrary
      -- |don't want words that span the entire line length
      l <- choose (maximum (map length w) + 1, 200)
      return $ WordList (toStrLn w) l

toStrLn :: [String] -> String
toStrLn wl =
   unwords wl ++ "\n"

prop_cleanUpLines :: WordList -> Property
prop_cleanUpLines wl =
   let s = wrds wl
       l = len wl
       initialNewLines = elemIndices '\n' s
       cleanedNewLines = elemIndices '\n' (cleanUp s l)
   -- | if there are HTML entities to be escaped, it will introduce a shift
   in unescape s == s ==>
      cleanedNewLines `union` initialNewLines == cleanedNewLines

prop_cleanUpSpacesToLines :: WordList -> Property
prop_cleanUpSpacesToLines wl =
   let s = wrds wl
       l = len wl
   -- | if there are HTML entities to be escaped, it will introduce a shift
   in unescape s == s ==>
      findIndices (`elem` " \n") (cleanUp s l) == findIndices (`elem` " \n") s

prop_indentStringHasWidth :: Int -> String -> String -> Property
prop_indentStringHasWidth w prefix s =
   length prefix < w ==>
      all (\l -> length l <= w) $ lines (indentString w prefix s)

prop_indentStringInsertsPrefix :: Int -> String -> String -> Property
prop_indentStringInsertsPrefix w p s =
   length p < w && '\n' `notElem` p ==>
      all (isPrefixOf p) $ lines (indentString w p s)

unitTests :: TestTree
unitTests = testGroup "Unit tests" [unescapeTests, hrefsTests, parseConfigTests]

unescapeTests :: TestTree
unescapeTests = testGroup "unescape"
   [ testCase "unescapes empty" $
      unescape "" @?= ""
   , testCase "unescapes nothing" $
      unescape "& unescape amp nothing;" @?= "& unescape amp nothing;"
   , testCase "unescapes &amp;" $
      unescape "& unescape &amp; me;" @?= "& unescape & me;"
   , testCase "unescapes &lt; and &gt;" $
      unescape "& unescape &lt;me&gt;;" @?= "& unescape <me>;"
   ]

hrefsTests :: TestTree
hrefsTests =
   testGroup "hrefs" [testCase "no hrefs in empty" $ hrefs "" @?= []]

parseConfigTests :: TestTree
parseConfigTests = testGroup "parseConfig"
   [ testCase "parseConfig empty" $
      assocs (parseConfig "") @?= []
   , testCase "parseConfig dummy" $
      assocs (parseConfig "foo: bar\nstupid is stupid") @?= [("foo", "bar")]
   , testCase "parseConfig spaces" $
      assocs (parseConfig " foo : bar \n  stupid = really stupid  ") @?=
      [("foo", "bar"), ("stupid", "really stupid")]
   ]
