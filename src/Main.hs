--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

import Control.Monad          (when, unless)
import Control.Monad.Reader   (ReaderT, ask, runReaderT)
import Control.Monad.Trans    (liftIO)
import Text.Printf            (printf)
import System.Environment     (getArgs)
import Data.Char              (ord)
import Data.List              (intercalate, elemIndices)
import Data.Maybe             (isJust)
import System.IO

import System.Process         (readProcess)
import System.Console.ANSI    (clearLine)
import Data.Map               ((!))

import Rascal.Constants
import Rascal.Utils
import Rascal.Types
import Rascal.Conf
import Rascal.API

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
   let lnks = links $ listing l
   in bold ++ "\n--=| /r/" ++ name l ++ " |=--\n\n" ++ reset ++
      -- the -4 comes from letterizeLines
      letterizeLines (map (`showLink` (width - 4)) lnks)

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


-- |open nth link in a listing in given width
open :: NamedListing -> Int -> ReaderT RuntimeConf IO ()
open nl@(NamedListing _ _ (Listing l _)) n = do
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
      putStrLn $ cleanUp (selfText ln) w
      if null refs
      then waitKey w
      else do
         putStr "\n"
         mapM_ (putStrLn . showRef) $ zip [1..] refs
         message "press link number to open or a key to continue" w
         openRefs refs w

-- |display all comments of an article in a subreddit
openComments :: String -> Link -> ReaderT RuntimeConf IO ()
openComments subreddit ln = do
   conf <- ask
   let w = textWidth conf
       csort = commentSort conf
   when (numComments ln > 0) $ liftIO $ do
      (Comments cll) <- getComments subreddit (drop 3 (uid ln)) csort (userAgent conf)
      putStrLn ""
       -- the first is OriginalArticle, the length is always 2
      unless (null cll) $ do
         let allComments = showCommentListing w "" (author ln) (cll !! 1)
         if pageComments conf
         then doPageComments (textHeight conf - 2) 0 w allComments
         else mapM_ putStrLn allComments
      waitKey w

doPageComments :: Int -> Int -> Int -> [String] -> IO ()
doPageComments _ _ _ [] = return ()
doPageComments _ _ _ [_] = return ()   -- should never happen...
doPageComments height shown w allComments@(a:b:remaining)
   | shown > height = waitKey w >> doPageComments height 0 w allComments
   | otherwise = do
      let nbLines = 2 + sum (map (length . elemIndices '\n') [a, b])
          empty = height - shown
      shown' <- if empty > nbLines
               then return shown
               else do
                  waitKey w
                  -- '\b' is backspace, since even clearLine does not enforce
                  -- clearing the key typed for input
                  putStr "\b"
                  return 0
      putStrLn a
      putStrLn b
      doPageComments height (shown' + nbLines) w remaining

-- |open requested hrefs as urls and stop if anything else
openRefs :: [String] -> Int -> IO ()
openRefs l w = do
   c <- getChar
   clearLine
   let n = ord c - ord '1'
   when (0 <= n && n < length l) $ do
      openUrl (l !! n) w
      openRefs l w

main ::  IO ()
main = do
   hSetBuffering stdin NoBuffering
   args <- getArgs
   columns <- readProcess "tput" ["cols"] []
   nbLines <- readProcess "tput" ["lines"] []
   conf <- getUserConfig ".rascalrc" defaultConf
   let width = read columns
       height = read nbLines
       subreddit | null args = conf ! "subreddit"
                 | otherwise = head args
       lSort  = conf ! "linkSort"
       cSort  = conf ! "commentSort"
       uAgent = conf ! "userAgent"
       pComments = reads (conf ! "pageComments")
       pComments' | null pComments = True    -- TODO get from defaultConf
                  | otherwise = fst $ head pComments
       conf' = RuntimeConf width height cSort lSort pComments'
   list <- getListing lSort subreddit 0 Nothing uAgent
   runReaderT (displayListing list >> loop list) conf'

-- |show possible commands
displayCommands :: Int -> IO ()
displayCommands =
   message $ intercalate "/" (map makeCmd availableSorts) ++
      "/⟨m⟩ore links/⟨s⟩witch subreddit/open ⟨A-Y⟩/⟨q⟩uit"

-- |main event loop
loop :: NamedListing -> ReaderT RuntimeConf IO ()
loop l = do
   cmd <- liftIO getChar
   liftIO $ putStr "\b"
   liftIO clearLine
   conf <- ask
   case cmd of
      's' -> do
         subreddit <- liftIO $ do
            putStr "\nsubreddit to switch to: "
            hFlush stdout
            getLine
         list <- liftIO $ getListing (linkSort conf) subreddit 0 Nothing (userAgent conf)
         displayListing list
         loop list
      'm' -> if isJust (after (listing l))
            then do
               let subreddit = takeWhile (/= ' ') (name l)
                   sort = drop 4 (dropWhile (/= ' ') (name l))
                   cnt = count l + length (links (listing l))
                   aftr = after $ listing l
               list <- liftIO $ getListing sort subreddit cnt aftr (userAgent conf)
               displayListing list
               loop list
            else do
               liftIO $ message "no more posts…" (textWidth conf)
               loop l
      -- is this one of the sort options?
      x | isJust (getFullSort x) -> do
         list <- let (Just sort) = getFullSort x
                in liftIO $ getListing sort (takeWhile (/=' ') (name l)) 0 Nothing (userAgent conf)
         displayListing list
         loop list
      -- 25 elements displayed max
        | x `elem` ['A'..'Y'] -> do
         open l (ord x - ord 'A')
         loop l
      'q' -> return ()
      _ -> loop l
