{-# LANGUAGE OverloadedStrings #-}

module Push where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Time

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as J
import qualified Network.WebSockets as WS

import qualified Todo as M

type MessageChan = Chan Message

data MessageType
  = TodoCreated
  | TodoUpdated
  | TodoDeleted
  deriving (Show)

instance J.ToJSON MessageType where
  toJSON = J.toJSON . show

data Message
  = Message MessageType M.Todo
  deriving (Show)

instance J.ToJSON Message where
  toJSON (Message mty todo) = J.object ["type" .= mty, "todo" .= body]
    where
      typeHeader = J.object ["type" .= mty]
      body = case mty of
        TodoDeleted -> J.object ["_id" .= M.todo_id todo]
        _ -> J.toJSON todo

mkMessageChan :: IO MessageChan
mkMessageChan = newChan

publish :: MessageChan -> Message -> IO ()
publish = writeChan

subscribe :: MessageChan -> WS.WebSockets WS.Hybi10 ()
subscribe chan = do
  sink <- WS.getSink
  myChan <- liftIO $ dupChan chan
  liftIO $ forkIO $ forever $ do
    msg <- readChan myChan
    WS.sendSink sink $ WS.DataMessage $ WS.Binary (J.encode msg)
  return ()

