module Rascal.Conf where

import Data.Map            (Map, union, fromList)
import Control.Monad       (liftM)
import Data.Char           (isSpace)
import Data.List           (dropWhileEnd)

import System.Directory    (doesFileExist, getHomeDirectory)
import System.FilePath     ((</>))

type Key   = String
type Value = String
type Conf  = Map Key Value

-- |Reads a file and parses to a Map String String.
readConfig :: FilePath -> IO Conf
readConfig path = liftM parseConfig (readFile path)

-- |Parses a string into a list of Configuration types.
parseConfig :: String -> Conf
parseConfig = fromList . map getKeyValue . lines

-- |Turns a line into a key, value pair
getKeyValue :: String -> (Key, Value)
getKeyValue line =
   let (key, _:value) = break (`elem` ":=") line in
      (stripWhite key, stripWhite value)

stripWhite :: String -> String
stripWhite = dropWhile isSpace . dropWhileEnd isSpace

getUserConfig :: String -> [(String, String)] -> IO Conf
getUserConfig fileName defaultOptions = do
   home <- getHomeDirectory
   let userConfFile = home </> fileName
       defaultConf = fromList defaultOptions
   hasUserConf <- doesFileExist userConfFile
   if hasUserConf
   then do
      userConf <- readConfig userConfFile
      -- |union will take left over right if key is defined twice
      return $ userConf `union` defaultConf
   else return defaultConf
