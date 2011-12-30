{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application.Hxournal.NetworkClipboard.Server.Type
import Application.Hxournal.NetworkClipboard.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "hxournalclip-server"
  acid <- openLocalState M.empty 
  warpDebug 7800 (HxournalclipServer acid)