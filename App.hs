{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module App (
  parseStartupOption,
  makeSiteConf,
  StartupOption(..),
  routes
) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.List as List
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Char8 as C
import Control.Applicative
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

data StartupOption
  = StartupOption {
    opt_host :: String,
    opt_port :: Int,
    opt_soys :: [String],
    opt_mongoHost :: String,
    opt_mongoNeedAuth :: Bool
  }

data SiteConf
  = SiteConf {
    sc_soyConf :: Soy.RenderConfig,
    sc_mongoPipe :: Mongo.Pipe
  }

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
              ]
    setPort = \ p opts -> opts { opt_port = read p }
    setHost = \ h opts -> opts { opt_host = h }
    addTemplateFiles = \ f opts -> opts { opt_soys = f : opt_soys opts }
    setMongoHost = \ h opts -> opts { opt_mongoHost = h }
    setMongoNeedAuth = \ opts -> opts { opt_mongoNeedAuth = True }
    defaultOption = StartupOption {
      opt_host = "127.0.0.1",
      opt_port = 9000,
      opt_soys = [],
      opt_mongoHost = "127.0.0.1",
      opt_mongoNeedAuth = False
    }
    usageHeader = "Usage: Main [OPTIONS...]"

makeSiteConf option = do
  soyConf <- Soy.setup $ Soy.addFiles (opt_soys option)
  mongoPipe <- Mongo.runIOE $ Mongo.connect (Mongo.host (opt_mongoHost option))
  if opt_mongoNeedAuth option
    then do
      dbUn <- getEnv "OPENSHIFT_MONGODB_DB_USERNAME"
      dbPw <- getEnv "OPENSHIFT_MONGODB_DB_PASSWORD"
      eiResult <- Mongo.access mongoPipe Mongo.master "NOL" $ do
        Mongo.auth (T.pack dbUn) (T.pack dbPw)
      case eiResult of
        Right True -> return ()
        _ -> error (show eiResult)
    else return ()
  return $ SiteConf soyConf mongoPipe

data Todo
  = Todo {
    todo_id :: Maybe String,
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

type AppRouteM a = RouteT Url (ReaderT SiteConf (ServerPartT IO)) a
type AppM a = ReaderT SiteConf (ServerPartT IO) a

renderSoy :: [Soy.Identifier] -> [(Soy.Identifier, J.Value)] -> AppRouteM Response
renderSoy templateName json = do
  --liftIO $ print json
  soyConf <- asks sc_soyConf
  eiOut <- runErrorT $ Soy.render templateName json soyConf
  case eiOut of
    Left e -> badRequest $
      toResponse (List.intercalate "." (map T.unpack templateName) ++
                  ": " ++ show e)
    Right out -> do --liftIO $ putStrLn (L.unpack out)
                    ok $ toResponseBS (C.pack "text/html")
                                      (T.encodeUtf8 (L.fromStrict out))

createTodo :: Todo -> AppM ()
createTodo todo = do
  pipe <- asks sc_mongoPipe
  Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.insert "todos" todoDoc
  -- XXX: handle mongo failure here
  return ()
  where
    J.Object todoJSON = J.toJSON todo
    todoJSON' = removeIdField todoJSON
    todoDoc = J.toBson todoJSON'
    removeIdField = HM.delete "_id"

fetchSomeTodos :: Int -> AppM [Todo]
fetchSomeTodos count = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    todoDocCursor <- Mongo.find $ (Mongo.select [] "todos") {
      Mongo.limit = fromIntegral count
    }
    todoDocs <- Mongo.rest todoDocCursor
    return $ map (justSucc . J.fromJSON . J.Object . J.toAeson) todoDocs
  --liftIO $ print eiResult
  case eiResult of
    Left e -> error (show e) -- ?
    Right xs -> return xs

justSucc (J.Success x) = x

fetchTodoCount :: AppM Int
fetchTodoCount = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.count (Mongo.select [] "todos")
  case eiResult of
    Left e -> error (show e)
    Right v -> return v

deleteTodo :: String -> AppM ()
deleteTodo _id = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.deleteOne $ (Mongo.select ["_id" := mkObjId _id] "todos")
  --liftIO $ print (_id, eiResult)
  case eiResult of
    Left e -> error (show e) -- ?
    Right _ -> return ()
  where
    mkObjId xs = let (w32, _):_ = readHex (take 8 xs)
                     (w64, _):_ = readHex (drop 8 xs)
                  in Mongo.ObjId (Mongo.Oid w32 w64)

routeShowAllTodos :: AppRouteM Response
routeShowAllTodos = do
  todos <- lift $ fetchSomeTodos 5
  numTodos <- lift $ fetchTodoCount
  renderSoy ["todo", "home"] [ ("todos", J.toJSON todos)
                             , ("numTodos", J.toJSON numTodos)
                             ]

routeCreateTodo :: AppRouteM Response
routeCreateTodo = do
  methodM POST
  eiContent <- getDataFn $ body $ look "content"
  case eiContent of
    Left e -> badRequest $ toResponse (unlines e)
    Right content -> do
      --liftIO $ putStrLn content
      lift $ createTodo (Todo Nothing content)
      redirectUrl Home

routeDeleteTodo :: String -> AppRouteM Response
routeDeleteTodo _id = do
  lift $ deleteTodo _id
  redirectUrl Home

noImpl :: String -> ServerPartT IO Response
noImpl wat = notFound $ toResponse $ "Not Found: " ++ wat

routes = do
  let thisDomain = ""
      assetsRoot = "./assets"
  decodeBody myPolicy
  msum [ dir "favicon.ico" $ notFound (toResponse ())
       , do eiRoute <- implSite_ thisDomain "" site
            case eiRoute of
              Left _ -> mzero
              Right m -> return m
       , dir "assets" $ serveDirectory DisableBrowsing [] assetsRoot
       , do uri <- liftM rqUri askRq
            lift (noImpl uri)
       ]
  where
    myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

site :: Site Url (ReaderT SiteConf (ServerPartT IO) Response)
site = setDefault Home $ mkSitePI (runRouteT todoRoute)

todoRoute :: Url -> AppRouteM Response
todoRoute url = case url of
  Home -> routeShowAllTodos
  Create -> routeCreateTodo
  Delete _id -> routeDeleteTodo _id

data Url
  = Home
  | Create
  | Delete String
  deriving (Generic, Show)

instance PathInfo Url

redirectUrl :: Url -> AppRouteM Response
redirectUrl url = do
  actualUrl <- showURL url
  seeOther actualUrl (toResponse ())

