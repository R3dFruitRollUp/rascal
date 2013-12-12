--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

{-# LANGUAGE OverloadedStrings #-}
-- allow Text objects directly as strings

import Control.Applicative
import Data.Version
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
   url :: String,
   created :: Int,
   numComments :: Int
}

data Listing = Listing [Link]

instance FromJSON Link where
   parseJSON (Object o) = do
      datum <- o .: "data"
      etitle <- datum .: "title"
      Link (unescape etitle)
           <$> datum .: "author"
           <*> datum .: "score"
           <*> datum .: "is_self"
           <*> datum .: "url"
           <*> datum .: "created_utc"
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

showListing :: Listing -> Int -> String
showListing (Listing l) width =
   unlines (map (`showLink` width) l)

-- |Poor man's HTML entities unescaping
unescape :: String -> String
unescape [] = []
unescape ('&':'a':'m':'p':';':xs) = '&':xs
unescape ('&':'l':'t':';':xs) = '<':xs
unescape ('&':'g':'t':';':xs) = '>':xs
unescape (x:xs) = x:unescape xs

getNew :: String -> IO Listing
getNew subreddit =
   curlAeson parseJSON "GET" ("http://www.reddit.com/r/" ++ subreddit ++ "/new.json")
      [CurlUserAgent userAgent] noData

-- ensure no burst above 30 requests/min
main ::  IO ()
main = do
   args <- getArgs
   columns <- readProcess "tput" ["cols"] []
   listing <- getNew $ if length args == 1 then head args else "scrolls"
   let width = read columns in
      putStr $ showListing listing width
