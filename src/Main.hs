--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

import Control.Monad       (when)
import Text.Printf         (printf)
import System.Environment  (getArgs)
import Data.Char           (ord)
import Data.Maybe          (isJust)
import System.IO
import Control.Exception   (catch, handle)

import Data.Aeson          (parseJSON)
import Network.Curl.Aeson  (curlAeson, noData, CurlAesonException)
import Network.Curl.Opts   (CurlOption(CurlUserAgent))
import System.Process      (readProcess)
import System.Console.ANSI (clearLine)

import Rascal.Constants
import Rascal.Utils
import Rascal.Types

-- we do not use Show because we depend on an IO generated width
showLink :: Link -> Int -> String
showLink l width =
   let titlewidth = width - 34
       self = if isSelf l
              then green ++ "♦"
              else " "
       color = if score l == 0
               then blue
               else red in
      let format = printf " %%s%%3d%%s%%s %%-%d.%ds  %%20.20s  %%s%%3d%%s "
                   titlewidth titlewidth in
         printf format color (score l) self reset (title l) (author l)
                magenta (numComments l) reset

showListing :: NamedListing -> Int -> String
showListing l width =
   let (Listing links) = listing l in
      bold ++ "\n--=| /r/" ++ name l ++ " |=--\n\n" ++ reset ++
      -- the -4 comes from letterizeLines
      letterizeLines (map (`showLink` (width - 4)) links)

-- TODO different color if OP?
showComment :: Int -> String -> String -> String -> Comment -> [String]
showComment width prefix addedPrefix futurePrefix c =
   let prefix' = prefix ++ futurePrefix
       initialIndent = indentString width prefix $ printf
         "%s─ %.20s (%s%d%s|%s%d%s)" addedPrefix (cauthor c) red (ups c)
         reset blue (downs c) reset
       commentBlock = indentString width prefix' (unescape (body c)) in
      (prefix ++ "│ "):(initialIndent ++ init commentBlock):
         showCommentListing width prefix' (children c)

showCommentListing :: Int -> String -> CommentListing -> [String]
showCommentListing width prefix (CommentListing cl) =
   case cl of
      [] -> []
      _ -> concatMap (showComment width prefix "├" "│ ") (init cl) ++
          showComment width prefix "└" "  " (last cl)

-- |print a listing on screen and ask for a command
displayListing :: NamedListing -> Int -> IO ()
displayListing l w = do
   catch
      (putStrLn $ showListing l w)
      handleCurlAesonException
   displayCommands w

-- |get posts according to selection in argument's subreddit as a listing
getListing :: String -> String -> IO NamedListing
getListing select subreddit = do
   l <- let apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                     "/" ++ select ++ ".json" in
      curlAeson parseJSON "GET" apiurl [CurlUserAgent userAgent] noData
   return $ NamedListing (subreddit ++ " -- " ++ select) l

-- |print error message if there is a cURL exception
handleCurlAesonException :: CurlAesonException -> IO ()
handleCurlAesonException e = do
   putStrLn $ red ++ "Caught exception:" ++ reset
   print e
   putStrLn "maybe given subreddit does not exist…"

-- |open nth link in a listing in given width
open :: NamedListing -> Int -> Int -> IO ()
open nl n w =
   handle handleCurlAesonException $ let (Listing l) = listing nl in
      -- n >= 0 by construction on call of open, but...
      when (0 <= n && n < length l) $ let ln = (l !! n) in
         if isSelf ln
         then do
            openSelf ln w
            let subreddit = takeWhile (/=' ') (name nl) in
               openComments subreddit ln w
            displayListing nl w
         else
            openUrl (link ln) w

-- |display a self link, with its contained hrefs
openSelf :: Link -> Int -> IO ()
openSelf ln w = do
   message "" w
   putStrLn $ unescape (selfText ln)
   let refs = hrefs (selfHtml ln) in
      if null refs
      then waitKey w
      else do
         putStr "\n"
         showRefs $ zip [1..] refs
         message "press link number to open or a key to continue" w
         openRefs refs w

-- |display all comments of an article in a subreddit
openComments :: String -> Link -> Int -> IO ()
openComments subreddit ln w =
   when (numComments ln > 0) $ do
      comm <- getComments subreddit (drop 3 (uid ln))
      putStrLn ""
      let (Comments cll) = comm in
          -- the first is OriginalArticle, the length is always 2
         mapM_ putStrLn $ showCommentListing w "" (cll !! 1) -- ^FIXME handle error
      waitKey w

getComments :: String -> String -> IO Comments
getComments subreddit article =
   let apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                "/comments/" ++ article ++ ".json?sort=new" in
      curlAeson parseJSON "GET" apiurl [CurlUserAgent userAgent] noData

-- |open requested hrefs as urls and stop if anything else
openRefs :: [String] -> Int -> IO ()
openRefs l w = do
   c <- getChar
   clearLine
   let n = ord c - ord '1' in
      when (0 <= n && n < length l) $ do
         openUrl (l !! n) w
         openRefs l w

showRefs :: [(Int, String)] -> IO ()
showRefs [] = return ()
showRefs ((n, u):xs) = do
   putStrLn $ " [" ++ yellow ++ show n ++ reset ++ "] " ++ blue ++ u ++ reset
   showRefs xs

-- GET comments
-- r/subreddit/comments/article_id36.json?context=0&sort=(new|hot)

-- GET search
-- r/subreddit/search.json?syntax=plain&q=&sort=

-- GET subscribed
-- subreddits/mine/subscriber.json

-- POST login
-- https://ssl.reddit.com/api/login?api_type=json&user=&passwd=&rem=true

-- POST comment
-- api/comment?api_type=json&text=&thing=&uh=

-- POST save
-- api/(un)save?id=&uh=

-- POST submit
-- ...

-- POST vote
-- api/vote?id=&dir=&uh= (dir -1, 0, 1)

main ::  IO ()
main = do
   hSetBuffering stdin NoBuffering
   args <- getArgs
   columns <- readProcess "tput" ["cols"] []
   list <- getListing "new" $ if length args == 1 then head args else "haskell"
   let width = read columns in
      displayListing list width >> loop list width

-- |show possible commands
displayCommands :: Int -> IO ()
displayCommands =
   message $ foldl makeCmd "" availableSorts ++ "⟨s⟩witch subreddit/open ⟨A-Y⟩"

-- |main event loop
loop :: NamedListing -> Int -> IO ()
loop l w = do
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
         loop list w
      -- is this one of the sort options?
      x | isJust (getFullSort x) -> do
         list <- let (Just sort) = getFullSort x in
            getListing sort $ takeWhile (/=' ') $ name l
         displayListing list w
         loop list w
      -- 25 elements displayed max
        | x `elem` ['A'..'Y'] -> do
         open l (ord x - ord 'A') w
         loop l w
      _ -> return ()
