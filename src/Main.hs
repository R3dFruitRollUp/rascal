--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

import Control.Monad       (when, unless)
import Control.Applicative ((<$>))
import Text.Printf         (printf)
import System.Environment  (getArgs)
import Data.Char           (ord)
import Data.Maybe          (isJust)
import System.IO
import Control.Exception   (catch)

import Data.Aeson          (parseJSON)
import Network.Curl.Aeson  (curlAeson, noData, CurlAesonException)
import Network.Curl.Opts   (CurlOption(CurlUserAgent))
import System.Process      (readProcess)
import System.Console.ANSI (clearLine)
import Data.Map            ((!))

import Rascal.Constants
import Rascal.Utils
import Rascal.Types
import Rascal.Conf

-- we do not use Show because we depend on (an IO generated) width
showLink :: Link -> Int -> String
showLink l width =
   let titlewidth = width - 34
       self = if isSelf l
              then green ++ "♦"
              else " "
       color = if score l == 0
               then blue
               else red
       format = printf " %%s%%3d%%s%%s %%-%d.%ds  %%20.20s  %%s%%3d%%s "
                titlewidth titlewidth
   in printf format color (score l) self reset (title l) (author l)
      magenta (numComments l) reset

showListing :: NamedListing -> Int -> String
showListing l width =
   let (Listing links) = listing l
   in bold ++ "\n--=| /r/" ++ name l ++ " |=--\n\n" ++ reset ++
      -- the -4 comes from letterizeLines
      letterizeLines (map (`showLink` (width - 4)) links)

showComment :: Int -> String -> String -> String -> String -> Comment -> [String]
showComment width prefix addedPrefix futurePrefix op
   (Comment cauthor ups downs _ body children) =
   let prefix' = prefix ++ futurePrefix
       author' = if cauthor == op
                 then green ++ cauthor ++ reset
                 else cauthor
       header = printf "%s─ %.20s (%s%d%s|%s%d%s)" addedPrefix author' red
         ups reset blue downs reset
       headerBlock = indentString width prefix header
       commentBlock = indentString width prefix' (unescape body)
   in (prefix ++ "│ "):(headerBlock ++ init commentBlock):
      showCommentListing width prefix' op children

showComment _ _ _ _ _ OriginalArticle =
   []

showCommentListing :: Int -> String -> String -> CommentListing -> [String]
showCommentListing width prefix op (CommentListing cl) =
   case cl of
      [] -> []
      _ -> concatMap (showComment width prefix "├" "│ " op) (init cl) ++
          showComment width prefix "└" "  " op (last cl)

-- |print a listing on screen and ask for a command
displayListing :: NamedListing -> Int -> IO ()
displayListing l w = do
   putStrLn $ showListing l w
   displayCommands w

-- |get posts according to selection in argument's subreddit as a listing
getListing :: String -> String -> IO NamedListing
getListing select subreddit = do
   let select' = if select `notElem` map snd availableSorts
                 then snd . head $ availableSorts
                 else select
       apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                "/" ++ select' ++ ".json"
   NamedListing (subreddit ++ " -- " ++ select') <$> catch
      (do
         l <- curlAeson parseJSON "GET" apiurl [CurlUserAgent userAgent] noData
         return $! l)
      (handleCurlAesonException emptyListing)

-- |print error message if there is a cURL exception
-- TODO better error message, depending on e
handleCurlAesonException :: a -> CurlAesonException -> IO a
handleCurlAesonException x e = do
   putStrLn $ red ++ "Caught exception:" ++ reset
   print e
   putStrLn "maybe given subreddit does not exist…"
   return x

-- |open nth link in a listing in given width
open :: NamedListing -> Int -> Int -> String -> IO ()
open nl@(NamedListing _ (Listing l)) n w cs =
   -- n >= 0 by construction on call of open, but...
   when (0 <= n && n < length l) $
      let ln = (l !! n)
      in do
         if isSelf ln
         then
            openSelf ln w
         else
            openUrl (link ln) w
         let subreddit = takeWhile (/=' ') (name nl)
         openComments subreddit ln w cs
         displayListing nl w

-- |display a self link, with its contained hrefs
openSelf :: Link -> Int -> IO ()
openSelf ln w = do
   message "" w
   putStrLn $ unescape (selfText ln)
   let refs = hrefs (selfHtml ln)
   if null refs
   then waitKey w
   else do
      putStr "\n"
      mapM_ putStrLn $ showRefs $ zip [1..] refs
      message "press link number to open or a key to continue" w
      openRefs refs w

-- |display all comments of an article in a subreddit
openComments :: String -> Link -> Int -> String -> IO ()
openComments subreddit ln w csort =
   when (numComments ln > 0) $ do
      (Comments cll) <- getComments subreddit (drop 3 (uid ln)) csort
      putStrLn ""
       -- the first is OriginalArticle, the length is always 2
      unless (null cll) $
         mapM_ putStrLn $ showCommentListing w "" (author ln) (cll !! 1)
      waitKey w

-- |request comments for a given article
getComments :: String -> String -> String -> IO Comments
getComments subreddit article csort = do
   let select = if csort `notElem` map snd availableSorts
                then snd . head $ availableSorts
                else csort
       apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                "/comments/" ++ article ++ ".json?sort=" ++ select
   catch (do
      c <- curlAeson parseJSON "GET" apiurl [CurlUserAgent userAgent] noData
      return $! c) $ handleCurlAesonException emptyComments

-- |open requested hrefs as urls and stop if anything else
openRefs :: [String] -> Int -> IO ()
openRefs l w = do
   c <- getChar
   clearLine
   let n = ord c - ord '1'
   when (0 <= n && n < length l) $ do
      openUrl (l !! n) w
      openRefs l w

showRefs :: [(Int, String)] -> [String]
showRefs [] = []
showRefs ((n, u):xs) =
   (" [" ++ yellow ++ show n ++ reset ++ "] " ++ blue ++ u ++ reset):showRefs xs

main ::  IO ()
main = do
   hSetBuffering stdin NoBuffering
   args <- getArgs
   columns <- readProcess "tput" ["cols"] []
   conf <- getUserConfig ".rascalrc" defaultConf
   let width = read columns
       subreddit = if length args == 1
                   then head args
                   else conf ! "subreddit"
       linkSort  = conf ! "linkSort"
       commentSort  = conf ! "commentSort"
   list <- getListing linkSort subreddit
   displayListing list width
   loop list width commentSort

-- |show possible commands
displayCommands :: Int -> IO ()
displayCommands =
   message $ foldl makeCmd "" availableSorts ++ "⟨s⟩witch subreddit/open ⟨A-Y⟩"

-- |main event loop
loop :: NamedListing -> Int -> String -> IO ()
loop l w cs = do
   cmd <- getChar
   clearLine
   case cmd of
      's' -> do
         putStrLn ""
         putStr "subreddit to switch to: "
         hFlush stdout
         subreddit <- getLine
         list <- getListing "new" subreddit
         displayListing list w
         loop list w cs
      -- is this one of the sort options?
      x | isJust (getFullSort x) -> do
         list <- let (Just sort) = getFullSort x
                in getListing sort $ takeWhile (/=' ') $ name l
         displayListing list w
         loop list w cs
      -- 25 elements displayed max
        | x `elem` ['A'..'Y'] -> do
         open l (ord x - ord 'A') w cs
         loop l w cs
      _ -> return ()
