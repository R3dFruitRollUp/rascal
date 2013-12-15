--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

{-# LANGUAGE OverloadedStrings #-}
-- allow Text objects directly as strings, used for JSON parsing

import Control.Applicative (empty, (<$>), (<*>))
import Control.Monad       (when)
import Data.Version        (showVersion)
import Text.Printf         (printf)
import System.Environment  (getArgs)
import System.Info         (os)
import Data.Char           (ord)
import System.IO
import Control.Exception   (catch)

import Data.Aeson          (parseJSON, FromJSON, Value(Object), (.:), (.:?), (.!=))
import Network.Curl.Aeson  (curlAeson, noData, CurlAesonException)
import Network.Curl.Opts   (CurlOption(CurlUserAgent))
import System.Process      (callProcess, readProcess)
import System.Console.ANSI

import Paths_rascal        (version)

userAgent :: String
userAgent = "rascal/" ++ showVersion version ++ " by soli"

red :: String
red  = setSGRCode [SetColor Foreground Dull Red]
green :: String
green = setSGRCode [SetColor Foreground Dull Green]
yellow :: String
yellow  = setSGRCode [SetColor Foreground Dull Yellow]
blue :: String
blue  = setSGRCode [SetColor Foreground Dull Blue]
magenta :: String
magenta  = setSGRCode [SetColor Foreground Dull Magenta]
cyan :: String
cyan  = setSGRCode [SetColor Foreground Dull Cyan]
reset :: String
reset = setSGRCode [Reset]
bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

data Link = Link {
   title :: String,
   author :: String,
   score :: Int,
   isSelf :: Bool,
   link :: String,
   -- created :: Int,
   -- uid :: String,
   numComments :: Int,
   selfHtml :: String,
   selfText :: String
}

newtype Listing = Listing [Link]

data NamedListing = NamedListing {
   name :: String,
   listing :: Listing
}

-- |json parser for 'Link'
instance FromJSON Link where
   parseJSON (Object o) = do
      datum <- o .: "data"
      etitle <- datum .: "title"
      Link (unescape etitle)
           <$> datum .: "author"
           <*> datum .: "score"
           <*> datum .: "is_self"
           <*> datum .: "url"
           -- <*> datum .: "created_utc"
           -- <*> datum .: "name"
           <*> datum .: "num_comments"
           <*> datum .:? "selftext_html" .!= ""
           <*> datum .: "selftext"
   parseJSON _ = empty

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

-- |json parser for 'Listing'
instance FromJSON Listing where
   parseJSON (Object o) = do
      datum <- o .: "data"
      Listing <$> datum .: "children"
   parseJSON _ = empty

-- |add capital letter and separate by newlines
numberLines :: [String] -> String
numberLines l =
   unlines $ zipWith (\c s -> ' ':c:" |" ++ s) ['A'..'Z'] l

showListing :: NamedListing -> Int -> String
showListing l width =
   let (Listing links) = listing l in
      bold ++ "\n--=| /r/" ++ name l ++ " |=--\n\n" ++ reset ++
      -- the -4 comes from numberLines
      numberLines (map (`showLink` (width - 4)) links)

displayListing :: NamedListing -> Int -> IO ()
displayListing l w = do
   putStrLn $ showListing l w
   message "⟨n⟩ew/⟨h⟩ot/open ⟨#⟩" w

-- Poor man's HTML entities unescaping
unescape :: String -> String
unescape [] = []
unescape ('&':'a':'m':'p':';':xs) = '&':xs
unescape ('&':'l':'t':';':xs) = '<':xs
unescape ('&':'g':'t':';':xs) = '>':xs
unescape (x:xs) = x:unescape xs

-- |get new posts in argument's subreddit as a listing
getNew :: String -> IO NamedListing
getNew = getListing "new"

-- |get top posts in argument's subreddit as a listing
getHot :: String -> IO NamedListing
getHot = getListing "hot"

-- |get posts according to selection in argument's subreddit as a listing
getListing :: String -> String -> IO NamedListing
getListing select subreddit = do
   l <- let apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                     "/" ++ select ++ ".json" in
      catch
         (curlAeson parseJSON "GET" apiurl [CurlUserAgent userAgent] noData)
         listingForError
   return $ NamedListing (subreddit ++ " -- " ++ select) l

-- |return empty listing if there is a cURL exception
listingForError :: CurlAesonException -> IO Listing
listingForError e = do
   print e
   return $ Listing []

-- |extract links from some HTML
hrefs :: String -> [String]
hrefs ('h':'r':'e':'f':'=':'"':s) =
   let (u, r) = break (== '"') s in
      (u:hrefs r)
hrefs (_:xs) = hrefs xs
hrefs "" = []


-- |open nth link in a listing in given width
open :: NamedListing -> Int -> Int -> IO ()
open nl n w =
   let (Listing l) = listing nl in
      -- n >= 0 by construction on call of open, but...
      when (0 <= n && n < length l) $ let ln = (l !! n) in
         if isSelf ln
         then do
            openSelf ln w
            displayListing nl w
         else
            openUrl (link ln) w

-- |display a self link, with its contained hrefs
openSelf :: Link -> Int -> IO ()
openSelf ln w = do
   message "" w
   putStrLn $ selfText ln
   let refs = hrefs (selfHtml ln) in
      if null refs
      then do
         message "press a key to continue" w
         getChar
         clearLine
         return ()
      else do
         putStr "\n"
         showRefs $ zip [1..] refs
         message "press link number to open or a key to continue" w
         openRefs refs w

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

-- |open an url in a platform independent way
openUrl :: String -> Int -> IO ()
openUrl u w = do
   message ("opening '" ++ u ++ "'…") w
   case os of
    "darwin"  -> callProcess "open" [u]
    "linux"   -> callProcess "xdg-open" [u, "&"] -- getEnv BROWSER ???
    "mingw32" -> callProcess "start" ["", u]

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
   list <- getNew $ if length args == 1 then head args else "scrolls"
   let width = read columns in
      displayListing list width >> loop list width

-- |main event loop
loop :: NamedListing -> Int -> IO ()
loop l w = do
   cmd <- getChar
   clearLine
   case cmd of
      'n' -> do
         list <- getNew $ takeWhile (/=' ') $ name l
         displayListing list w
         loop list w
      'h' -> do
         list <- getHot $ takeWhile (/=' ') $ name l
         displayListing list w
         loop list w
      -- 25 elements displayed by default
      x | x `elem` ['A'..'Y'] -> do
         open l (ord x - ord 'A') w
         loop l w
      _ -> return ()
