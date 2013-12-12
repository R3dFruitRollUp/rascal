--
-- RASCAL, a Haskell cli reddit client
-- Copyright (c) 2013 Sylvain Soliman <Sylvain.Soliman@gmail.com>
-- MIT License, see LICENSE
--

{-# LANGUAGE OverloadedStrings #-}
-- allow Text objects directly as strings

import Control.Applicative
import Data.Version

import Data.Aeson
import Network.Curl.Aeson
import Network.Curl.Opts

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
} deriving(Show)

data Listing = Listing [Link] deriving (Show)

instance FromJSON Link where
   parseJSON (Object o) = do
      datum <- o .: "data"
      Link <$> datum .: "title"
           <*> datum .: "author"
           <*> datum .: "score"
           <*> datum .: "is_self"
           <*> datum .: "url"
           <*> datum .: "created_utc"
           <*> datum .: "num_comments"
   parseJSON _ = empty

instance FromJSON Listing where
   parseJSON (Object o) = do
      datum <- o .: "data"
      Listing <$> datum .: "children"
   parseJSON _ = empty

-- ensure no burst above 30 requests/min
main ::  IO ()
main = do
   t <- curlAeson parseJSON "GET" "http://www.reddit.com/r/scrolls/new.json"
      [CurlUserAgent userAgent] noData :: IO Listing
   print t
