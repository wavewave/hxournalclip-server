{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.NetworkClipboard.Server.Yesod where 

import Yesod hiding (update)
import Network.Wai
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString as S
import Application.HXournal.NetworkClipboard.Type
import Data.Acid
import Data.Attoparsec as P
import Data.Aeson as A
import Data.UUID
import Application.HXournal.NetworkClipboard.Server.Type

mkYesod "HXournalClipServer" [parseRoutes|
/ HomeR GET
/listhxournalclip  ListHXournalClipR GET
/uploadhxournalclip  UploadHXournalClipR POST
/hxournalclip/#UUID HXournalClipR 
|]

instance Yesod HXournalClipServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage HXournalClipServer FormMessage where
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


getListHXournalClipR :: Handler RepHtmlJson
getListHXournalClipR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadHXournalClipR :: Handler RepHtmlJson
postUploadHXournalClipR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result HXournalClipInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddHXournalClip minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))



handleHXournalClipR :: UUID -> Handler RepHtmlJson
handleHXournalClipR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getHXournalClipR name
    "PUT" -> putHXournalClipR name
    "DELETE" -> deleteHXournalClipR name
    x -> error ("No such action " ++ show x ++ " in handlerHXournalClipR")

getHXournalClipR :: UUID -> Handler RepHtmlJson
getHXournalClipR idee = do 
  liftIO $ putStrLn "getHXournalClipR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryHXournalClip idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putHXournalClipR :: UUID -> Handler RepHtmlJson
putHXournalClipR idee = do 
  liftIO $ putStrLn "putHXournalClipR called"
  acid <- return.server_acid =<< getYesod
  _wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result HXournalClipInfo) of 
        Success minfo -> do 
          if idee == hxournalclip_uuid minfo
            then do r <- liftIO $ update acid (UpdateHXournalClip minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "hxournalclipname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalClipInfo))

deleteHXournalClipR :: UUID -> Handler RepHtmlJson
deleteHXournalClipR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteHXournalClip idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
