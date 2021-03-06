-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Wireless
-- Copyright   :  (c) Jose Antonio Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose Antonio Ortega Ruiz
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor reporting ESSID and link quality for wireless interfaces
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Wireless (wirelessConfig, runWireless)  where

import Plugins.Monitors.Common
import IWlib
import Text.Printf
import System.Console.GetOpt

data WiOpts = WiOpts 
  { iconBase :: Maybe String
  , numIcons :: Integer
  }

defaultOpts :: WiOpts 
defaultOpts = WiOpts 
  { iconBase = Nothing
  , numIcons = 0
  }

options :: [OptDescr (WiOpts -> WiOpts)]
options =
  [ Option "I" ["iconbase"] (ReqArg (\x o -> o { iconBase = Just x }) "") ""
  , Option "N" ["numicons"] (ReqArg (\x o -> o { numIcons = read x }) "") ""
  ]

parseOpts :: [String] -> IO WiOpts 
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

wirelessConfig :: IO MConfig
wirelessConfig =
  mkMConfig "<essid> <quality>" ["essid", "quality", "qualitybar", "qualityvbar", "qualityicons"]

getStrLength :: Integer -> Integer
getStrLength n = floor (logBase 10 (fromIntegral n)) + 1

getStrInd :: Integer -> Float -> Integer
getStrInd n qlty = round (qlty/100*(fromIntegral (n-1)))+1

getIcon :: Maybe String -> Integer -> Float -> String
getIcon Nothing _ _ = ""
getIcon _ 0 _ = ""
getIcon (Just s) n qlty = do
  let l = getStrLength n
      suf = printf ("%0"++(show l)++"d") (getStrInd n qlty)
  "<icon=/" ++ s ++ suf ++ ".xbm/>"

runWireless :: String -> [String] -> Monitor String
runWireless iface args = do
  opts <- io $ parseOpts args
  wi <- io $ getWirelessInfo iface
  na <- getConfigValue naString
  let essid = wiEssid wi
      qlty = fromIntegral $ wiQuality wi
      e = if essid == "" then na else essid
  ep <- showWithPadding e
  q <- if qlty >= 0
       then showPercentWithColors (qlty / 100)
       else showWithPadding ""
  qb <- showPercentBar qlty (qlty / 100)
  qvb <- showVerticalBar qlty (qlty / 100)
  qi <- showWithPadding (getIcon (iconBase opts) (numIcons opts) qlty)
  parseTemplate [ep, q, qb, qvb, qi]
--unWireless _ = getConfigValue naString
