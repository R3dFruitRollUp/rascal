{-# LANGUAGE OverloadedStrings #-}
-- allow Text objects directly as strings
--
-- RASCAL, a Haskell cli reddit client

import Data.Aeson
import Network.Curl.Aeson
import Control.Applicative
import Network.Curl.Opts

version :: [Char]
version = "1.0"

user_agent :: [Char]
user_agent = "rascal/" ++ version ++ " by soli"

data Link = Link {
   domain :: String,
   author :: String,
   score :: Int,
   url :: String,
   title :: String,
   num_comments :: Int
} deriving(Show)

data Listing = Listing [Link] deriving (Show)

instance FromJSON Link where
   parseJSON (Object o) = do
      d_ata <- o .: "data"
      Link <$> d_ata .: "domain"
           <*> d_ata .: "author"
           <*> d_ata .: "score"
           <*> d_ata .: "url"
           <*> d_ata .: "title"
           <*> d_ata .: "num_comments"
   parseJSON _ = empty

instance FromJSON Listing where
   parseJSON (Object o) = do
      d_ata <- o .: "data"
      Listing <$> d_ata .: "children"
   parseJSON _ = empty

-- ensure no burst above 30 requests/min
main ::  IO ()
main = do
   t <- curlAeson parseJSON "GET" "http://www.reddit.com/r/scrolls/new.json"
      [CurlUserAgent user_agent] noData :: IO Listing
   print t
