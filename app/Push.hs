{-# LANGUAGE OverloadedStrings #-}

module Push where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Time
import qualified Data.HashMap.Strict as HM

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as J
import qualified Network.WebSockets as WS

import qualified Todo as M

type MessageChan = Chan Message

data Message
  = TodoCreated M.Todo
  | TodoUpdated M.Todo
  | TodoDeleted M.TodoId
  deriving (Show)

instance J.ToJSON Message where
  toJSON msg = J.object ["type" .= mty, "todo" .= body]
    where
      mty :: String
      body :: J.Value
      (mty, body) = case msg of
        TodoCreated todo -> ("todo-created", J.toJSON todo)
        TodoUpdated todo -> ("todo-updated", J.toJSON todo)
        TodoDeleted _id -> ("todo-deleted", J.object ["_id" .= _id])

mkMessageChan :: IO MessageChan
mkMessageChan = newChan

-- TODO: better pub/sub abstraction
publish :: MessageChan -> Message -> IO ()
publish chan msg = do
  --putStrLn $ "inside pub: " ++ show msg
  writeChan chan msg

subscribe :: MessageChan -> WS.WebSockets WS.Hybi10 ()
subscribe chan = do
  myChan <- liftIO $ dupChan chan
  tid <- liftIO myThreadId
  --liftIO $ putStrLn $ "subscr: " ++ show tid
  WS.catchWsError (readPushLoop myChan) (onDisconn tid)
  where
    onDisconn tid e = case fromException e of
      Just WS.ConnectionClosed -> do
        liftIO $ putStrLn $ "closed: " ++ show tid

readPushLoop :: MessageChan -> WS.WebSockets WS.Hybi10 ()
readPushLoop chan = do
  msg <- liftIO $ readChan chan
  WS.send $ WS.DataMessage $ WS.Text (J.encode msg)
  readPushLoop chan

