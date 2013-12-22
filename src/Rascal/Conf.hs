--
-- Looked at the several existing config-file parsing libs
-- they were all overkill
-- |This module does the strict minimum needed parsing INI-style
-- config files
--

module Rascal.Conf where

import Data.Map            (Map, union, fromList)
import Data.Char           (isSpace)
import Data.List           (dropWhileEnd)

import System.Directory    (doesFileExist, getHomeDirectory)
import System.FilePath     ((</>))

type Key   = String
type Value = String
type Conf  = Map Key Value

-- |Parses a string into a Map String String
parseConfig :: String -> Conf
parseConfig = fromList . map getKeyValue . filter configLine . lines

-- |keep only lines that will lead to some configuration element
-- stripping out comments, empty lines, etc.
configLine :: String -> Bool
configLine l =
   not (null l) && head l /= '#' && (':' `elem` l || '=' `elem` l)

-- |Turns a line into a key, value pair
getKeyValue :: String -> (Key, Value)
getKeyValue line =
   let (key, _:value) = break (`elem` ":=") line in
      (stripWhite key, stripWhite value)

-- |drop leading and ending white space
stripWhite :: String -> String
stripWhite = dropWhile isSpace . dropWhileEnd isSpace

-- |search for fileName in the user's home directory and combine it with
-- default options to provide a Conf map
getUserConfig :: String -> [(String, String)] -> IO Conf
getUserConfig fileName defaultOptions = do
   home <- getHomeDirectory
   let userConfFile = home </> fileName
       defaultConf = fromList defaultOptions
   hasUserConf <- doesFileExist userConfFile
   if hasUserConf
   then do
      userConf <- readFile userConfFile
      -- |union will take left over right if key is defined twice
      return $ parseConfig userConf `union` defaultConf
   else return defaultConf
