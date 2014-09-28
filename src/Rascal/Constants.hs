-- |Various constants for rascal
module Rascal.Constants where

import Data.Version        (showVersion)

import System.Console.ANSI

import Paths_rascal        (version)

-- |colors and more
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

-- |available sort options
availableSorts :: [(Char, String)]
availableSorts = [
     ('n', "new")
   , ('h', "hot")
   , ('t', "top")
   , ('c', "controversial")
   ]

-- |default configuration options as an association list
defaultConf :: [(String, String)]
defaultConf = [
     ("subreddit", "haskell")
   , ("linkSort", "new")
   , ("commentSort", "new")
   , ("pageComments", "true")
   , ("userAgent", "rascal/" ++ showVersion version ++ " by soli")
   ]
