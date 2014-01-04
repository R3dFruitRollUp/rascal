-- |Various utilities for rascal
module Rascal.Utils where

import System.Info         (os)
import Control.Exception   (handle)
import Data.List           (elemIndices)

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
   let (u, r) = break (== '"') s
   in (u:hrefs r)
hrefs (_:xs) = hrefs xs
hrefs "" = []

-- |pretty print sort name with initial or selected letter highlighted
makeCmd :: (Char, String) -> String
makeCmd (c, cmd) | c `notElem` cmd = '⟨':c:'⟩':tail cmd
                 | otherwise = let (hd, _:tl) = break (== c) cmd
                               in hd ++ '⟨':c:'⟩':tl

-- |get full sort name from initial
getFullSort :: Char -> Maybe String
getFullSort = (`lookup` availableSorts)

-- |add a prefix (e.g. indent) on the left for a string to be printed in given
-- width note, because of unlines, a \n ends each line
indentString :: Int -> String -> String -> String
indentString width prefix s =
   let lineLength = width - length prefix
       strings = concatMap (splitAt' lineLength) (lines s)
   in unlines $ map (prefix ++) strings

-- |split a string to a list of substring of length <= n
splitAt' :: Int -> String -> [String]
splitAt' 0 s = [s]
splitAt' n s =
   let (s1, s2) = splitAt n s
   in if null s2
      then [s1]
      else let (s1', s2') = splitAtLastSpace s1 s2
           in s1':splitAt' n s2'

splitAtLastSpace :: String -> String -> (String, String)
splitAtLastSpace s1 s2 =
   let spaces = elemIndices ' ' s1
   in if null spaces
      then (s1, s2)
      else let (s1', _:s2') = splitAt (last spaces) s1
           in (s1', s2' ++ s2)

-- | unescape and add newlines in order not to cut words
cleanUp :: String -> Int -> String
cleanUp s w =
      unlines (concatMap (splitAt' w) ((lines . unescape) s))

-- |display an informative message
message :: String -> Int -> IO ()
message s w =
   let col = cyan ++ reset
       msg = if null s
             then col
             else "--[" ++ cyan ++ s ++ reset ++ "]"
       l = length msg - length col
   in do
      putStrLn ""
      putStr msg
      putStrLn $ replicate (w - l) '-'

-- |wait for a key press
waitKey :: Int -> IO ()
waitKey width = do
   message "press a key to continue" width
   _ <- getChar
   clearLine
   return ()

-- |open an url in a platform independent way
openUrl :: String -> Int -> IO ()
openUrl u w = do
   message ("opening '" ++ u ++ "'…") w
   handle (\e -> print (e :: IOError)) $
      case os of
       "darwin"  -> callProcess "open" ["-g", u]   -- open in the background
       "linux"   -> callProcess "xdg-open" [u]     -- getEnv BROWSER ???
       "mingw32" -> callProcess "start" ["", u]
       _         -> return ()

showRef :: (Int, String) -> String
showRef (n, u) =
   " [" ++ yellow ++ show n ++ reset ++ "] " ++ blue ++ u ++ reset
