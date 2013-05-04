{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Url where

import Control.Applicative
import Web.Routes

import qualified Model as M

data ApiUrl
  = TodoResource (Maybe M.TodoId)
  deriving (Show)

data Url
  = ListTodo
  | CreateTodo
  | DeleteTodo M.TodoId
  | UpdateTodo M.TodoId
  deriving (Show, Generic)

instance PathInfo Url

instance PathInfo ApiUrl where
  toPathSegments (TodoResource (Just s)) = "todo" : toPathSegments s
  toPathSegments (TodoResource Nothing) = ["todo"]
  fromPathSegments = TodoResource <$
                     segment "todo" <*>
                     (parseJust <|> parseNothing)
    where
      parseJust = do
        xs <- fromPathSegments
        return $ Just xs
      parseNothing = return Nothing

