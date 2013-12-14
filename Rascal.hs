--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

{-# LANGUAGE OverloadedStrings #-}
-- allow Text objects directly as strings, used for JSON parsing

import Control.Applicative
import Data.Version
import Data.Char
import Text.Printf
import System.Process

import Data.Aeson
import Network.Curl.Aeson
import Network.Curl.Opts
import System.Environment

import Paths_rascal

userAgent :: String
userAgent = "rascal/" ++ showVersion version ++ " by soli"

data Link = Link {
   title :: String,
   author :: String,
   score :: Int,
   isSelf :: Bool,
   -- url :: String,
   -- created :: Int,
   -- uid :: String,
   numComments :: Int
}

newtype Listing = Listing [Link]

data NamedListing = NamedListing {
   name :: String,
   listing :: Listing
}

instance FromJSON Link where
   parseJSON (Object o) = do
      datum <- o .: "data"
      etitle <- datum .: "title"
      Link (unescape etitle)
           <$> datum .: "author"
           <*> datum .: "score"
           <*> datum .: "is_self"
           -- <*> datum .: "url"
           -- <*> datum .: "created_utc"
           -- <*> datum .: "name"
           <*> datum .: "num_comments"
   parseJSON _ = empty

-- we do not use Show because we depend on an IO generated width
showLink :: Link -> Int -> String
showLink l width =
   let titlewidth = width - 34
       self = if isSelf l then '♦' else ' ' in
      let format = printf " %%3d%%c %%-%d.%ds  %%20.20s  %%3d " titlewidth titlewidth in
         printf format (score l) self (title l) (author l) (numComments l)

instance FromJSON Listing where
   parseJSON (Object o) = do
      datum <- o .: "data"
      Listing <$> datum .: "children"
   parseJSON _ = empty

-- | prepend formatted int before a string
addNumber :: Int -> String -> String
addNumber =
   printf " %2d |%s"

-- add number and separate by newlines
numberLines :: [String] -> String
numberLines l =
   unlines $ zipWith addNumber [1..] l

showListing :: NamedListing -> Int -> String
showListing l width =
   let (Listing links) = listing l in
      "--=| /r/" ++ name l ++ " |=--\n\n" ++
      -- the -5 comes from numberLines
      numberLines (map (`showLink` (width - 5)) links)

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
      curlAeson parseJSON "GET" apiurl [CurlUserAgent userAgent] noData
   return $ NamedListing (subreddit ++ " -- " ++ select) l


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

-- ensure no burst above 30 requests/min
main ::  IO ()
main = do
   args <- getArgs
   columns <- readProcess "tput" ["cols"] []
   list <- getNew $ if length args == 1 then head args else "scrolls"
   let width = read columns in
      loop list width

loop :: NamedListing -> Int -> IO ()
loop l w = do
   putStrLn $ showListing l w
   putStrLn "[n]ew/[h]ot/open [#]"
   cmd <- getLine
   case cmd of
      'n':_ -> do
         list <- getNew $ takeWhile (/=' ') $ name l
         loop list w
      'h':_ -> do
         list <- getHot $ takeWhile (/=' ') $ name l
         loop list w
      n@(x:_) | isDigit x -> do
         putStrLn n
         loop l w
      _ -> return ()

