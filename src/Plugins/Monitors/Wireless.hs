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
import System.Console.GetOpt

data WiOpts = WiOpts 
  { iconBase :: Maybe String
  , numIcons :: Float 
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

runWireless :: [String] -> Monitor String
runWireless (iface:args) = do
  opts <- io $ parseOpts args
  ni <- getConfigValue numIcons
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
  qi <- showPercentWithColors (qlty / ni)
  parseTemplate [ep, q, qb, qvb, qi]
runWireless _ = getConfigValue naString
