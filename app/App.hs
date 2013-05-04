{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts,
    FlexibleInstances #-}

module App (
  parseStartupOption,
  makeSiteConf,
  StartupOption(..),
  routes
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import System.Environment
import System.Console.GetOpt
import Numeric

import Database.MongoDB ((=:), Field((:=)))
import qualified Database.MongoDB as Mongo
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as J
import qualified Data.AesonBson as J
import qualified Language.Soy as Soy
import Happstack.Server
import Web.Routes
import Web.Routes.Happstack

import qualified Model as M
import qualified Route as R
import qualified Url as U
import qualified Push as P
import Conf

parseStartupOption = do
  args <- getArgs
  case getOpt Permute options args of
    (optTrans, _, []) -> return $ foldl (flip id) defaultOption optTrans
    (_,        _, es) -> ioError $ userError $ concat es ++
                         usageInfo usageHeader options
  where
    options = [ Option [] ["port"] (ReqArg setPort "PORT") "bind at PORT"
              , Option [] ["host"] (ReqArg setHost "HOST") "bind at HOST"
              , Option [] ["mongo-host"] (ReqArg setMongoHost "HOST")
                  "connect to mongoDB at HOST"
              , Option [] ["mongo-need-auth"] (NoArg setMongoNeedAuth)
                  "set mongoDB need auth"
              , Option [] ["template-files"] (ReqArg addTemplateFiles "PATHS")
                  "add soy template file from PATHS"
              , Option [] ["assets-root"] (ReqArg setAssetsRoot "PATH")
                  "serve asset files in PATH"
              ]
    setPort = \ p opts -> opts { opt_port = read p }
    setHost = \ h opts -> opts { opt_host = h }
    addTemplateFiles = \ f opts -> opts { opt_soys = f : opt_soys opts }
    setMongoHost = \ h opts -> opts { opt_mongoHost = h }
    setMongoNeedAuth = \ opts -> opts { opt_mongoNeedAuth = True }
    setAssetsRoot = \ r opts -> opts { opt_assetsRoot = r }
    defaultOption = StartupOption {
      opt_host = "127.0.0.1",
      opt_port = 9000,
      opt_soys = [],
      opt_mongoHost = "127.0.0.1",
      opt_mongoNeedAuth = False,
      opt_assetsRoot = "./assets"
    }
    usageHeader = "Usage: Main [OPTIONS...]"

makeSiteConf option = do
  soyConf <- Soy.setup $ Soy.addFiles (opt_soys option)
  pushChan <- P.mkMessageChan
  mongoPipe <- Mongo.runIOE $ do
    Mongo.connect (Mongo.host (opt_mongoHost option))
  let dbname = "NOL"
  if opt_mongoNeedAuth option
    then do
      dbUn <- getEnv "OPENSHIFT_MONGODB_DB_USERNAME"
      dbPw <- getEnv "OPENSHIFT_MONGODB_DB_PASSWORD"
      eiResult <- Mongo.access mongoPipe Mongo.master dbname $ do
        Mongo.auth (T.pack dbUn) (T.pack dbPw)
      case eiResult of
        Right True -> return ()
        _ -> error (show eiResult)
    else return ()
  return $ SiteConf soyConf option (M.DBInfo mongoPipe dbname pushChan)

routes = do
  let thisDomain = ""
  assetsRoot <- asks (Conf.opt_assetsRoot . Conf.sc_opt)
  decodeBody myPolicy
  msum [ dir "_" $      implSite thisDomain "" apiSite
       ,                implSite thisDomain "" pageSite
       ]
  where
    myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

pageSite :: Site U.Url (R.ConfM Response)
pageSite = setDefault U.ListTodo $ mkSitePI (runRouteT pageRoute)

pageRoute :: U.Url -> R.PageRouteM Response
pageRoute url = case url of
  U.ListTodo       -> R.pageShowAllTodos
  U.CreateTodo     -> R.pageCreateTodo
  U.UpdateTodo _id -> R.pageUpdateTodo _id
  U.DeleteTodo _id -> R.pageDeleteTodo _id

apiSite = mkSitePI (runRouteT apiRoute)

apiRoute (U.TodoResource mbId) = case mbId of
  Nothing -> msum [R.apiCreateTodo]
  Just _id -> msum [ R.apiUpdateTodo _id
                   , R.apiDeleteTodo _id
                   ]

