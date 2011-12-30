{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application.HXournal.NetworkClipboard.Server.Type
import Application.HXournal.NetworkClipboard.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

import Data.Xournal.Simple 
import Data.Strict.Tuple
import Data.UUID.V5 
import Application.HXournal.NetworkClipboard.Type 

teststroke = [Stroke "test" "test" 1.1 [ 0.0 :!: 1.0 ] ]

teststrokeinfo = HXournalClipInfo namespaceURL teststroke



main :: IO ()
main = do 
  putStrLn "hxournalclip-server"
  acid <- openLocalState (M.insert namespaceURL teststrokeinfo M.empty )
  warpDebug 7800 (HXournalClipServer acid)