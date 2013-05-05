{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Route where

import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as T

import Happstack.Server
import Web.Routes
import Web.Routes.Happstack
import qualified Language.Soy as Soy

import qualified Model as M
import qualified Url as U
import qualified Conf

type ConfM = ReaderT Conf.SiteConf (ServerPartT IO)
type PageRouteM = RouteT U.Url ConfM
type ApiRouteM = RouteT U.ApiUrl ConfM

renderSoy :: [Soy.Identifier] -> [(Soy.Identifier, J.Value)] ->
             PageRouteM Response
renderSoy templateName args = do
  --liftIO $ print json
  soyConf <- asks Conf.sc_soyConf
  eiOut <- runErrorT $ Soy.render templateName (args ++ additionalArgs) soyConf
  case eiOut of
    Left e -> badRequest $
      toResponse (List.intercalate "." (map T.unpack templateName) ++
                  ": " ++ show e)
    Right out -> do --liftIO $ putStrLn (L.unpack out)
                    ok $ toResponseBS (C.pack "text/html")
                                      (T.encodeUtf8 (L.fromStrict out))
  where
    additionalArgs = [("assetsTimestamp", "201305041623")]

runModel m = do
  dbInfo <- asks Conf.sc_dbInfo
  liftIO $ runReaderT m dbInfo

pageShowAllTodos :: PageRouteM Response
pageShowAllTodos = do
  todos <- runModel $ M.fetchSomeTodos 5
  numTodos <- runModel $ M.fetchTodoCount
  renderSoy ["todo", "servtmpl", "home"]
            [ ("todos", J.toJSON todos)
            , ("numTodos", J.toJSON numTodos)
            ]

pageCreateTodo :: PageRouteM Response
pageCreateTodo = do
  methodM POST
  eiContent <- getDataFn $ body $ look "content"
  case eiContent of
    Left e -> badRequest $ toResponse (unlines e)
    Right content -> do
      --liftIO $ putStrLn content
      runModel $ M.createTodo (M.Todo Nothing content)
      redirectUrl U.ListTodo

pageUpdateTodo :: String -> PageRouteM Response
pageUpdateTodo _id = msum [doUpdate, getUpdateForm]
  where
    getUpdateForm = do
      mbTodo <- runModel $ M.fetchTodo _id
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
          runModel $ M.updateTodo (M.Todo (Just _id) content)
          redirectUrl U.ListTodo

pageDeleteTodo :: String -> PageRouteM Response
pageDeleteTodo _id = do
  runModel $ M.deleteTodo _id
  redirectUrl U.ListTodo

noImpl :: FilterMonad Response m => String -> m Response
noImpl wat = notFound $ toResponse $ "Not Found: " ++ wat

getBody :: (MonadIO m, ServerMonad m) => m BL.ByteString
getBody = do
    req  <- askRq 
    mbBody <- liftIO $ takeRequestBody req 
    case mbBody of 
        Just body -> return . unBody $ body
        Nothing   -> return "" 

redirectUrl :: U.Url -> PageRouteM Response
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
  toContentType = const (C.pack "pagelication/json; charset=UTF-8")
  toMessage a = jr_xssPrefix a `BL.append` J.encode (jr_content a)

-- To be kept in sync with frontend js code :P
jSONXssPrefix = "for(;;){}"

jsonResponse v = toResponse (JSONResponse (J.toJSON v) jSONXssPrefix)
createdJSON = created . jsonResponse

apiCreateTodo :: ApiRouteM Response
apiCreateTodo = do
  methodM POST
  Just todo <- getJsonReq
  todo' <- runModel $ M.createTodo todo
  createdJSON todo'

-- Returning the updated todo. Although currently it's also trivial
-- to reuse the todo in the client side, in the future some complex fields
-- may require resending the model anyways.
apiUpdateTodo :: String -> ApiRouteM Response
apiUpdateTodo _id = do
  methodM PUT
  Just todo <- getJsonReqWithId _id
  runModel $ M.updateTodo todo
  ok $ jsonResponse todo

apiDeleteTodo :: String -> ApiRouteM Response
apiDeleteTodo _id = do
  methodM DELETE
  runModel $ M.deleteTodo _id
  ok $ toResponse ()

getJsonReq :: (J.FromJSON a, MonadIO m, ServerMonad m) => m (Maybe a)
getJsonReq = do
  rawBody <- getBody
  --liftIO $ print rawBody
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


