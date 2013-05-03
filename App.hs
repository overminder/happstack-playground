{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts,
    FlexibleInstances #-}

module App (
  parseStartupOption,
  makeSiteConf,
  StartupOption(..),
  routes
) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.List as List
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
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
    opt_mongoNeedAuth :: Bool,
    opt_assetsRoot :: String
  }

data SiteConf
  = SiteConf {
    sc_soyConf :: Soy.RenderConfig,
    sc_mongoPipe :: Mongo.Pipe,
    sc_opt :: StartupOption
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
  return $ SiteConf soyConf mongoPipe option

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
type AjaxRouteM a = RouteT AjaxUrl (ReaderT SiteConf (ServerPartT IO)) a
type AppM a = ReaderT SiteConf (ServerPartT IO) a

renderSoy :: [Soy.Identifier] -> [(Soy.Identifier, J.Value)] ->
             AppRouteM Response
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

createTodo :: Todo -> AppM Todo
createTodo todo = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.insert "todos" todoDoc
  -- XXX: handle mongo failure here
  case eiResult of
    Left e -> error (show e) -- ?
    Right oid -> let J.String oidT = J.aesonifyValue oid
                  in return $ todo { todo_id = Just (T.unpack oidT) }
  where
    J.Object todoJSON = J.toJSON todo
    todoJSON' = removeIdField todoJSON
    todoDoc = J.toBson todoJSON'

fetchSomeTodos :: Int -> AppM [Todo]
fetchSomeTodos count = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    todoDocCursor <- Mongo.find $ (Mongo.select [] "todos") {
      Mongo.limit = fromIntegral count
    }
    todoDocs <- Mongo.rest todoDocCursor
    return $ map docToHsVal todoDocs
  --liftIO $ print eiResult
  case eiResult of
    Left e -> error (show e) -- ?
    Right xs -> return xs

-- Some helper functions for mongo/aeson
justSucc (J.Success x) = x
removeIdField = HM.delete "_id"
docToHsVal = justSucc . J.fromJSON . J.Object . J.toAeson

mkObjId :: String -> Mongo.Value
mkObjId xs = let (w32, _):_ = readHex (take 8 xs)
                 (w64, _):_ = readHex (drop 8 xs)
              in Mongo.ObjId (Mongo.Oid w32 w64)

fetchTodo :: String -> AppM (Maybe Todo)
fetchTodo _id = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.findOne $ (Mongo.select ["_id" := mkObjId _id] "todos")
  case eiResult of
    Left e -> error (show e)
    Right mbTodo -> return $ fmap docToHsVal mbTodo

fetchTodoCount :: AppM Int
fetchTodoCount = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.count (Mongo.select [] "todos")
  case eiResult of
    Left e -> error (show e)
    Right v -> return v

updateTodo :: Todo -> AppM ()
updateTodo todo = do
  pipe <- asks sc_mongoPipe
  _ <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.replace (Mongo.select ["_id" := mkObjId idField] "todos")
                  (J.toBson json')
  return ()
  where
    J.Object json = J.toJSON $ todo
    json' = removeIdField json
    idField = fromJust $ todo_id todo

deleteTodo :: String -> AppM ()
deleteTodo _id = do
  pipe <- asks sc_mongoPipe
  eiResult <- Mongo.access pipe Mongo.master "NOL" $ do
    Mongo.deleteOne $ (Mongo.select ["_id" := mkObjId _id] "todos")
  --liftIO $ print (_id, eiResult)
  case eiResult of
    Left e -> error (show e) -- ?
    Right _ -> return ()

appShowAllTodos :: AppRouteM Response
appShowAllTodos = do
  todos <- lift $ fetchSomeTodos 5
  numTodos <- lift $ fetchTodoCount
  renderSoy ["todo", "servtmpl", "home"]
            [ ("todos", J.toJSON todos)
            , ("numTodos", J.toJSON numTodos)
            ]

appCreateTodo :: AppRouteM Response
appCreateTodo = do
  methodM POST
  eiContent <- getDataFn $ body $ look "content"
  case eiContent of
    Left e -> badRequest $ toResponse (unlines e)
    Right content -> do
      --liftIO $ putStrLn content
      lift $ createTodo (Todo Nothing content)
      redirectUrl Home

appUpdateTodo :: String -> AppRouteM Response
appUpdateTodo _id = msum [doUpdate, getUpdateForm]
  where
    getUpdateForm = do
      mbTodo <- lift $ fetchTodo _id
      case mbTodo of
        Nothing -> do
          -- XXX: This could acutally happen
          -- So it would be better to return a more helpful message
          notFound (toResponse ())
        Just todo -> do
          renderSoy ["todo", "servtmpl", "update"]
                    [("todo", J.toJSON todo)]
    doUpdate = do
      methodM POST
      eiContent <- getDataFn $ body $ look "content"
      case eiContent of
        Left e -> badRequest $ toResponse (unlines e)
        Right content -> do
          lift $ updateTodo (Todo (Just _id) content)
          redirectUrl Home

appDeleteTodo :: String -> AppRouteM Response
appDeleteTodo _id = do
  lift $ deleteTodo _id
  redirectUrl Home

noImpl :: FilterMonad Response m => String -> m Response
noImpl wat = notFound $ toResponse $ "Not Found: " ++ wat

routes = do
  let thisDomain = ""
  assetsRoot <- asks (opt_assetsRoot . sc_opt)
  decodeBody myPolicy
  msum [ dir "favicon.ico" $ notFound (toResponse ())
       , dir "a" $ do eiRoute <- implSite_ thisDomain "" ajaxSite
                      case eiRoute of
                        Left _ -> mzero
                        Right m -> return m
       , do eiRoute <- implSite_ thisDomain "" appSite
            case eiRoute of
              Left _ -> mzero
              Right m -> return m
       , dir "assets" $ serveDirectory DisableBrowsing [] assetsRoot
       , do uri <- liftM rqUri askRq
            lift (noImpl uri)
       ]
  where
    myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

appSite :: Site Url (ReaderT SiteConf (ServerPartT IO) Response)
appSite = setDefault Home $ mkSitePI (runRouteT appRoute)

appRoute :: Url -> AppRouteM Response
appRoute url = case url of
  Home -> appShowAllTodos
  Create -> appCreateTodo
  Update _id -> appUpdateTodo _id
  Delete _id -> appDeleteTodo _id

ajaxSite = mkSitePI (runRouteT ajaxRoute)

ajaxRoute url = case url of
  AjaxTodo mbId -> case mbId of
    Nothing -> msum [ajaxCreateTodo]
    Just _id -> msum [ ajaxUpdateTodo _id
                     , ajaxDeleteTodo _id
                     ]

data Url
  = Home
  | Create
  | Update String
  | Delete String
  deriving (Generic, Show)

instance PathInfo Url

data AjaxUrl
  = AjaxTodo (Maybe String)
  deriving (Show)

-- Custom instance here
instance PathInfo AjaxUrl where
  toPathSegments (AjaxTodo (Just s)) = "todo" : toPathSegments s
  toPathSegments (AjaxTodo Nothing) = ["todo"]
  fromPathSegments = AjaxTodo <$ segment "todo" <*> (parseJust <|> parseNothing)
    where
      parseJust = do
        xs <- fromPathSegments
        return $ Just xs
      parseNothing = return Nothing

ajaxCreateTodo :: AjaxRouteM Response
ajaxCreateTodo = do
  methodM POST
  Just todo <- getJsonReq
  todo' <- lift $ createTodo todo
  createdJSON todo'

-- Returning the updated todo. Although currently it's also trivial
-- to reuse the todo in the client side, in the future some complex fields
-- may require resending the model anyways.
ajaxUpdateTodo :: String -> AjaxRouteM Response
ajaxUpdateTodo _id = do
  methodM PUT
  Just todo <- getJsonReqWithId _id
  lift $ updateTodo todo
  ok $ jsonResponse todo

ajaxDeleteTodo :: String -> AjaxRouteM Response
ajaxDeleteTodo _id = do
  methodM DELETE
  lift $ deleteTodo _id
  ok $ toResponse ()

getJsonReq :: (J.FromJSON a, MonadIO m, ServerMonad m) => m (Maybe a)
getJsonReq = do
  rawBody <- getBody
  return (J.decode rawBody)

-- Some RESTful request don't provide a _id field (E.g. PUT).
-- Therefore we need to add it manually.
getJsonReqWithId :: (J.FromJSON a, MonadIO m, ServerMonad m) =>
                    String -> m (Maybe a)
getJsonReqWithId _id = do
  rawBody <- getBody
  let mbVal = J.decode rawBody
      addId (J.Object v) = J.Object (HM.insert "_id" (J.toJSON _id) v)
  case fmap (J.fromJSON . addId) mbVal of
    Just (J.Success x) -> return (Just x)
    _ -> return Nothing

getBody :: (MonadIO m, ServerMonad m) => m BL.ByteString
getBody = do
    req  <- askRq 
    mbBody <- liftIO $ takeRequestBody req 
    case mbBody of 
        Just body -> return . unBody $ body
        Nothing   -> return "" 

redirectUrl :: Url -> AppRouteM Response
redirectUrl url = do
  actualUrl <- showURL url
  seeOther actualUrl (toResponse ())

created = resp 201

data JSONResponse = 
  JSONResponse {
    jr_content :: J.Value,
    jr_xssPrefix :: BL.ByteString
  }

instance ToMessage JSONResponse where
  toContentType = const (C.pack "application/json; charset=UTF-8")
  toMessage a = jr_xssPrefix a `BL.append` J.encode (jr_content a)

-- To be kept in sync with frontend js code :P
jSONXssPrefix = "for(;;){}"

jsonResponse v = toResponse (JSONResponse (J.toJSON v) jSONXssPrefix)
createdJSON = created . jsonResponse

