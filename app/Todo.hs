{-# LANGUAGE OverloadedStrings #-}

module Todo where

import Control.Monad
import Control.Applicative
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as J

type TodoId = String

data Todo
  = Todo {
    todo_id :: Maybe TodoId,
    todo_content :: String
  }
  deriving (Show)

instance J.ToJSON Todo where
  toJSON (Todo _id content) = J.object ["_id" .= _id, "content" .= content]

-- Don't use DeriveGeneric here since we are using different attributes.
instance J.FromJSON Todo where
  parseJSON (J.Object v) = Todo <$>
                           v .: "_id" <*>
                           v .: "content"
  parseJSON _ = mzero

