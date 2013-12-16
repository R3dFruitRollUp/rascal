-- |Various utilities for rascal
module Rascal.Utils where

import Rascal.Constants

-- |add capital letter and separate by newlines at most 25 strings
-- |for a total of 4 chars
letterizeLines :: [String] -> String
letterizeLines l =
   unlines $ zipWith (\c s -> ' ':c:" |" ++ s) ['A'..'Y'] l

-- |Poor man's HTML entities unescaping
unescape :: String -> String
unescape [] = []
unescape ('&':'a':'m':'p':';':xs) = '&':xs
unescape ('&':'l':'t':';':xs) = '<':xs
unescape ('&':'g':'t':';':xs) = '>':xs
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
