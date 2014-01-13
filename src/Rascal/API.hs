module Rascal.API where

import Control.Applicative    ((<$>))
import Text.Printf            (printf)
import Control.Exception      (handle)

import Data.Aeson             (parseJSON, FromJSON)
import Network.Curl.Aeson     (curlAeson, noData, CurlAesonException, errorMsg, curlCode)
import Network.Curl.Opts      (CurlOption(CurlUserAgent))
import Network.Curl.Code      (CurlCode(CurlOK))

import Rascal.Constants
import Rascal.Types

-- |get posts according to selection in argument's subreddit as a listing
getListing :: String -> String -> Int -> Maybe String -> IO NamedListing
getListing select subreddit cnt aftr =
   let apiurl = "http://www.reddit.com/r/" ++ subreddit ++ "/%s.json?count="
                ++ show cnt ++ maybe "" ("&after=" ++) aftr
   in NamedListing (subreddit ++ " -- " ++ select) cnt <$>
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
handleCurlAesonException :: a -> CurlAesonException -> IO a
handleCurlAesonException x e = do
   putStrLn $ red ++ "Caught exception: " ++ reset ++ errorMsg e
   putStrLn $ if curlCode e == CurlOK
              then "(Might indicate a non-existing subreddit)"
              else "cURL code: " ++ (drop 4 . show . curlCode) e
   return x

-- |request comments for a given article
getComments :: String -> String -> String -> IO Comments
getComments subreddit article csort =
   let apiurl = "http://www.reddit.com/r/" ++ subreddit ++
                "/comments/" ++ article ++ ".json?sort=%s"
   in getThing apiurl csort emptyComments
