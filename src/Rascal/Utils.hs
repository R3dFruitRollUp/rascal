-- |Various utilities for rascal
module Rascal.Utils where

import Rascal.Constants

-- |add capital letter and separate by newlines at most 25 strings
-- for a total of 4 chars
letterizeLines :: [String] -> String
letterizeLines l =
   unlines $ zipWith (\c s -> ' ':c:" |" ++ s) ['A'..'Y'] l

-- |Poor man's HTML entities unescaping
unescape :: String -> String
unescape [] = []
unescape ('&':'a':'m':'p':';':xs) = '&':unescape xs
unescape ('&':'l':'t':';':xs) = '<':unescape xs
unescape ('&':'g':'t':';':xs) = '>':unescape xs
unescape (x:xs) = x:unescape xs

-- |extract links from some HTML
hrefs :: String -> [String]
hrefs ('h':'r':'e':'f':'=':'"':s) =
   let (u, r) = break (== '"') s in
      (u:hrefs r)
hrefs (_:xs) = hrefs xs
hrefs "" = []

-- |pretty print sort name with initial
makeCmd :: String -> (Char, String) -> String
makeCmd acc (s, sort) =
   acc ++ ('⟨':s:'⟩':tail sort) ++ "/"

-- |get full sort name from initial
getFullSort :: Char -> Maybe String
getFullSort = (`lookup` availableSorts)

-- |add a block indent on the left for a string to be printed in given width
-- for each n, 2 spaces are added. Note, because of unlines, a \n ends each
-- line
indentString :: Int -> String -> String -> String
indentString width prefix s =
   let lineLength = width - length prefix
       strings = concatMap (splitAt' lineLength) (lines s) in
      unlines $ map (prefix ++) strings

-- |split a string to a list of substring of length <= n
splitAt' :: Int -> String -> [String]
splitAt' n s =
   let (s1, s2) = splitAt n s in
      if null s2
      then [s1]
      else s1:splitAt' n s2
