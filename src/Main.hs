--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

import Control.Monad          (when, unless)
import Control.Monad.Reader   (ReaderT, ask, runReaderT)
import Control.Monad.Trans    (liftIO)
import Control.Applicative    ((<$>))
import Text.Printf            (printf)
import System.Environment     (getArgs)
import Data.Char              (ord)
import Data.Maybe             (isJust)
import System.IO
import Control.Exception      (handle)

import Data.Aeson             (parseJSON, FromJSON)
import Network.Curl.Aeson     (curlAeson, noData, CurlAesonException)
import Network.Curl.Opts      (CurlOption(CurlUserAgent))
import System.Process         (readProcess)
import System.Console.ANSI    (clearLine)
import Data.Map               ((!))

import Rascal.Constants
import Rascal.Utils
import Rascal.Types
import Rascal.Conf

-- we do not use Show because we depend on (an IO generated) width
showLink :: Link -> Int -> String
showLink l width =
   let titlewidth = width - 34
       self | isSelf l = green ++ "♦"
            | otherwise = " "
       color | score l == 0 = blue
             | otherwise = red
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
       author' | cauthor == op = green ++ cauthor ++ reset
               | otherwise = cauthor
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
displayListing :: NamedListing -> ReaderT RuntimeConf IO ()
displayListing l = do
   conf <- ask
   let w = textWidth conf
   liftIO $ do
      putStrLn $ showListing l w
      displayCommands w

-- |get posts according to selection in argument's subreddit as a listing
getListing :: String -> String -> IO NamedListing
getListing select subreddit =
   let apiurl = "http://www.reddit.com/r/" ++ subreddit ++ "/%s.json"
   in NamedListing (subreddit ++ " -- " ++ select) <$>
      getThing apiurl select emptyListing

-- |get posts or comments from an apiurl, a sort order and a default in case
-- of error
getThing :: FromJSON a => String -> String -> a -> IO a
getThing apiurl sort emptyThing =
   let sort' = if sort `notElem` map snd availableSorts
               then snd . head $ availableSorts
               else sort
       apiurl' = printf apiurl sort'
   in handle (handleCurlAesonException emptyThing) $ do
         l <- curlAeson parseJSON "GET" apiurl' [CurlUserAgent userAgent] noData
         return $! l

-- |print error message if there is a cURL exception
-- TODO better error message, depending on e
handleCurlAesonException :: a -> CurlAesonException -> IO a
handleCurlAesonException x e = do
   putStrLn $ red ++ "Caught exception:" ++ reset
   print e
   putStrLn "maybe given subreddit does not exist…"
   return x

-- |open nth link in a listing in given width
open :: NamedListing -> Int -> ReaderT RuntimeConf IO ()
open nl@(NamedListing _ (Listing l)) n = do
   conf <- ask
   -- n >= 0 by construction on call of open, but...
   when (0 <= n && n < length l) $
      let ln = (l !! n)
          w = textWidth conf
          subreddit = takeWhile (/=' ') (name nl)
      in do
         if isSelf ln
         then openSelf ln
         else liftIO $ openUrl (link ln) w
         openComments subreddit ln
         displayListing nl

-- |display a self link, with its contained hrefs
openSelf :: Link -> ReaderT RuntimeConf IO ()
openSelf ln = do
   conf <- ask
   let refs = hrefs (selfHtml ln)
       w = textWidth conf
   liftIO $ do
      message "" w
      putStrLn $ unescape (selfText ln)
      if null refs
      then waitKey w
      else do
         putStr "\n"
         mapM_ putStrLn $ showRefs $ zip [1..] refs
         message "press link number to open or a key to continue" w
         openRefs refs w

-- |display all comments of an article in a subreddit
openComments :: String -> Link -> ReaderT RuntimeConf IO ()
openComments subreddit ln = do
   conf <- ask
   let w = textWidth conf
       csort = commentSort conf
   when (numComments ln > 0) $ liftIO $ do
      (Comments cll) <- getComments subreddit (drop 3 (uid ln)) csort
      putStrLn ""
       -- the first is OriginalArticle, the length is always 2
      unless (null cll) $
         mapM_ putStrLn $ showCommentListing w "" (author ln) (cll !! 1)
      waitKey w

-- |request comments for a given article
getComments :: String -> String -> String -> IO Comments
getComments subreddit article csort =
   let apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                "/comments/" ++ article ++ ".json?sort=%s"
   in getThing apiurl csort emptyComments

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
       subreddit | null args = conf ! "subreddit"
                 | otherwise = head args
       linkSort  = conf ! "linkSort"
       cSort  = conf ! "commentSort"
       conf' = RuntimeConf width cSort
   list <- getListing linkSort subreddit
   runReaderT (displayListing list >> loop list) conf'

-- |show possible commands
displayCommands :: Int -> IO ()
displayCommands =
   message $ foldl makeCmd "" availableSorts ++ "⟨s⟩witch subreddit/open ⟨A-Y⟩"

-- |main event loop
loop :: NamedListing -> ReaderT RuntimeConf IO ()
loop l = do
   cmd <- liftIO getChar
   liftIO clearLine
   case cmd of
      's' -> do
         subreddit <- liftIO $ do
            putStr "\nsubreddit to switch to: "
            hFlush stdout
            getLine
         list <- liftIO $ getListing "new" subreddit
         displayListing list
         loop list
      -- is this one of the sort options?
      x | isJust (getFullSort x) -> do
         list <- let (Just sort) = getFullSort x
                in liftIO $ getListing sort $ takeWhile (/=' ') $ name l
         displayListing list
         loop list
      -- 25 elements displayed max
        | x `elem` ['A'..'Y'] -> do
         open l (ord x - ord 'A')
         loop l
      _ -> return ()
