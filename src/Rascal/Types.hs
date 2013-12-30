{-# LANGUAGE OverloadedStrings #-}
-- allow Text objects directly as strings, used for JSON parsing

module Rascal.Types where

import Control.Applicative (empty, (<$>), (<*>), (<|>))

import Data.Aeson
import Data.Vector         (toList)

import Rascal.Utils        (unescape)

-- | a reddit post, called Link in reddit's API documentation
data Link = Link {
   title :: String,
   author :: String,
   score :: Int,
   isSelf :: Bool,
   link :: String,
   -- created :: Int,
   uid :: String,
   numComments :: Int,
   selfHtml :: String,
   selfText :: String
}

data Listing = Listing
   { links :: [Link]
   , after :: Maybe String
   }

emptyListing :: Listing
emptyListing = Listing [] Nothing

data NamedListing = NamedListing
   { name :: String
   , count :: Int
   , listing :: Listing
}

newtype Comments = Comments [CommentListing] deriving (Show)

emptyComments :: Comments
emptyComments = Comments []

newtype CommentListing = CommentListing [Comment] deriving (Show)

data Comment = Comment
   { _cauthor :: String
   , _ups :: Int
   , _downs :: Int
   -- , created :: Int
   -- , edited :: Int (or false)
   , __bodyHtml :: String
   , _body :: String
   , _children :: CommentListing
   }
   | OriginalArticle deriving (Show)

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
           <*> datum .: "name"
           <*> datum .: "num_comments"
           <*> datum .:? "selftext_html" .!= ""
           <*> datum .: "selftext"
   parseJSON _ = empty

-- |json parser for 'Listing'
instance FromJSON Listing where
   parseJSON (Object o) = do
      datum <- o .: "data"
      Listing <$> datum .: "children"
              <*> datum .: "after"
   parseJSON _ = empty

-- |json parser for 'Comments'
instance FromJSON Comments where
   parseJSON (Array a) = Comments <$> mapM parseJSON (toList a)
   parseJSON _ = empty

instance FromJSON CommentListing where
   parseJSON (Object o) = do
      datum <- o .: "data"
      a <- datum .: "children"
      CommentListing <$> mapM parseJSON (toList a)
   parseJSON _ = empty

instance FromJSON Comment where
   parseJSON (Object o) =  do
      kind <- o .: "kind"
      if (kind :: String) == "t1"
      then do
         datum <- o .: "data"
         Comment <$> datum .: "author"
                 <*> datum .: "ups"
                 <*> datum .: "downs"
                 <*> datum .: "body_html"
                 <*> datum .: "body"
                 <*> (datum .: "replies" <|> return (CommentListing []))
      else
         return OriginalArticle
   parseJSON _ = empty

data RuntimeConf = RuntimeConf
   { textWidth :: Int
   , textHeight :: Int
   , commentSort :: String
   , linkSort :: String
   , pageComments :: Bool
   }
