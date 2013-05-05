{-# LANGUAGE ConstraintKinds, FlexibleContexts, OverloadedStrings #-}

module Model (
  module Todo,
  module Model
) where

import Todo
import qualified Push as P

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Numeric

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as J
import qualified Data.AesonBson as J
import Database.MongoDB ((=:), Field((:=)))
import qualified Database.MongoDB as M


--runMongo :: MonadIO m => M.Pipe -> T.Text -> m a -> m a
runMongo :: MonadDB m => M.Action m a -> m a
runMongo m = do
  pipe <- asks db_pipe
  name <- asks db_name
  eiResult <- M.access pipe M.master name m
  case eiResult of
    Left e -> error (show e) -- XXX: handle failure here?
    Right res -> return res

type MonadDB m = (MonadBaseControl IO m, Applicative m, Monad m,
                  MonadReader DBInfo m, MonadIO m)
data DBInfo
  = DBInfo {
    db_pipe :: M.Pipe,
    db_name :: T.Text,
    db_pushChan :: P.MessageChan
  }

createTodo :: (MonadDB m) => Todo -> m Todo
createTodo todo = do
  pushChan <- asks db_pushChan
  runMongo $ do
    table <- return "todo"
    rawOid <- M.insert table todoDoc
    let J.String oidText = J.aesonifyValue rawOid
    let newTodo = todo { todo_id = Just (T.unpack oidText) }
    liftIO $ P.publish pushChan $ P.TodoCreated newTodo
    return newTodo
  where
    J.Object todoJSON = J.toJSON todo
    todoJSON' = removeIdField todoJSON
    todoDoc = J.toBson todoJSON'

fetchSomeTodos :: (MonadDB m) => Int -> m [Todo]
fetchSomeTodos count = runMongo $ do
  table <- return "todo"
  todoDocCursor <- M.find $ (M.select [] table) {
    M.limit = fromIntegral count
  }
  todoDocs <- M.rest todoDocCursor
  return $ map docToHsVal todoDocs

fetchTodo :: (MonadDB m) => String -> m (Maybe Todo)
fetchTodo _id = runMongo $ do
  table <- return "todo"
  mbTodo <- M.findOne $ (M.select [idAttr := mkObjId _id] table)
  return $ fmap docToHsVal mbTodo

fetchTodoCount :: (MonadDB m) => m Int
fetchTodoCount = runMongo $ do
  table <- return "todo"
  M.count (M.select [] table)

updateTodo :: (MonadDB m) => Todo -> m ()
updateTodo todo = do
  pushChan <- asks db_pushChan
  runMongo $ do
    table <- return "todo"
    M.replace (M.select [idAttr := mkObjId idField] table)
              (J.toBson json')
    liftIO $ P.publish pushChan $ P.TodoUpdated todo
    return ()
  where
    J.Object json = J.toJSON $ todo
    json' = removeIdField json
    idField = fromJust $ todo_id todo

deleteTodo :: (MonadDB m) => String -> m ()
deleteTodo _id = do
  pushChan <- asks db_pushChan
  runMongo $ do
    table <- return "todo"
    M.deleteOne $ (M.select [idAttr := mkObjId _id] table)
    liftIO $ P.publish pushChan $ P.TodoDeleted _id

-- Some helper functions for mongo/aeson
removeIdField = HM.delete idAttr

docToHsVal = fromSucc . J.fromJSON . J.Object . J.toAeson
  where
    fromSucc (J.Success x) = x

mkObjId :: String -> M.Value
mkObjId xs = let (w32, _):_ = readHex (take 8 xs)
                 (w64, _):_ = readHex (drop 8 xs)
              in M.ObjId (M.Oid w32 w64)

idAttr = "_id"

