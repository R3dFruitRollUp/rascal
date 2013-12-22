-- |Various utilities for rascal
module Rascal.Utils where

import System.Info         (os)

import System.Process      (callProcess)
import System.Console.ANSI (clearLine)

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

-- |add a prefix (e.g. indent) on the left for a string to be printed in given
-- width note, because of unlines, a \n ends each line
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

-- |display an informative message
message :: String -> Int -> IO ()
message s w =
   let col = cyan ++ reset
       msg = if null s
             then col
             else "--[" ++ cyan ++ s ++ reset ++ "]"
       l = length msg - length col in do
      putStrLn ""
      putStr msg
      putStrLn $ replicate (w - l) '-'

-- |wait for a key press
waitKey :: Int -> IO ()
waitKey w = do
   message "press a key to continue" w
   _ <- getChar
   clearLine
   return ()

-- |open an url in a platform independent way
openUrl :: String -> Int -> IO ()
openUrl u w = do
   message ("opening '" ++ u ++ "'…") w
   case os of
    "darwin"  -> callProcess "open" [u]
    "linux"   -> callProcess "xdg-open" [u, "&"] -- getEnv BROWSER ???
    "mingw32" -> callProcess "start" ["", u]
    _         -> return ()
