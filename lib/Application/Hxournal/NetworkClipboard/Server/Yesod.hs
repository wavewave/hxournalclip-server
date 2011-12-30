{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.Hxournal.NetworkClipboard.Server.Yesod where 

import Yesod hiding (update)
import Network.Wai
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString as S
import Application.Hxournal.NetworkClipboard.Type
import Data.Acid
import Data.Attoparsec as P
import Data.Aeson as A
import Data.UUID
import Application.Hxournal.NetworkClipboard.Server.Type

mkYesod "HxournalclipServer" [parseRoutes|
/ HomeR GET
/listhxournalclip  ListHxournalclipR GET
/uploadhxournalclip  UploadHxournalclipR POST
/hxournalclip/#UUID HxournalclipR 
|]

instance Yesod HxournalclipServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage HxournalclipServer FormMessage where
  renderMessage _ _ = defaultFormMessage -}


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]


defhlet :: GGWidget m Handler ()
defhlet = [whamlet| <h1> HTML output not supported |]


getListHxournalclipR :: Handler RepHtmlJson
getListHxournalclipR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadHxournalclipR :: Handler RepHtmlJson
postUploadHxournalclipR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result HxournalclipInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddHxournalclip minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))



handleHxournalclipR :: UUID -> Handler RepHtmlJson
handleHxournalclipR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getHxournalclipR name
    "PUT" -> putHxournalclipR name
    "DELETE" -> deleteHxournalclipR name
    x -> error ("No such action " ++ show x ++ " in handlerHxournalclipR")

getHxournalclipR :: UUID -> Handler RepHtmlJson
getHxournalclipR idee = do 
  liftIO $ putStrLn "getHxournalclipR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryHxournalclip idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putHxournalclipR :: UUID -> Handler RepHtmlJson
putHxournalclipR idee = do 
  liftIO $ putStrLn "putHxournalclipR called"
  acid <- return.server_acid =<< getYesod
  _wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result HxournalclipInfo) of 
        Success minfo -> do 
          if idee == hxournalclip_uuid minfo
            then do r <- liftIO $ update acid (UpdateHxournalclip minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "hxournalclipname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HxournalclipInfo))

deleteHxournalclipR :: UUID -> Handler RepHtmlJson
deleteHxournalclipR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteHxournalclip idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
