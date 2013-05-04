{-# LANGUAGE ScopedTypeVariables, Rank2Types, TemplateHaskell, CPP,
             OverloadedStrings #-}
{-# OPTIONS -pgmP cpp #-}

import System.IO
import Control.Monad.Reader

import qualified Happstack.Server as H
import qualified Happstack.Server.SimpleHTTP as H
import qualified Happstack.Server.Internal.Monads as H
import qualified Happstack.Server.Wai as HW
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Application.Static as S
import qualified WaiAppStatic.Types as S
import Data.FileEmbed (embedDir)

import App

main = do
  option <- parseStartupOption
  siteConf <- makeSiteConf option
  let warpSetting = W.defaultSettings {
        W.settingsPort = portNum,
        W.settingsHost = W.Host hostIp
      }
      hostIp = opt_host option
      portNum = opt_port option

  hPutStrLn stderr $ "Trying to listen at" ++ show (hostIp, portNum)
  sock <- H.bindIPv4 hostIp portNum
  hPutStrLn stderr $ "Server started at " ++ show (hostIp, portNum)
  W.runSettingsSocket warpSetting sock $
    composeApp (runReaderT routes siteConf) staticApp

composeApp hApp wApp = \ wReq -> do
  hReq <- HW.convertRequest wReq
  mbHResp <- liftIO $ runHapp hApp hReq
  case mbHResp of
    Just hResp -> do
      additionalHeaders <- liftIO HW.standardHeaders
      return $ HW.convertResponse additionalHeaders hResp
    Nothing -> wApp wReq

runHapp :: (H.ToMessage b, Monad m, Functor m) =>
           H.ServerPartT m b -> H.Request -> m (Maybe H.Response)
runHapp sp req = runWebT $ H.runServerPartT sp req

#define TO_S(x) TO_S_(x)
#define TO_S_(x) #x

-- Serves static files by compiling them into the binary :P
staticApp = S.staticApp setting'
  where
    setting = S.embeddedSettings $(embedDir TO_S(ASSETS_DIR))
    lookup = S.ssLookupFile setting
    lookup' pieces = case pieces of
      x:xs | "assets" == S.fromPiece x -> lookup xs
      _ -> return S.LRNotFound
    setting' = setting {
      S.ssLookupFile = lookup',
      S.ssUseHash = True,
      S.ssListing = Nothing
    }

-- Copied from Happstack.Server.SimpleHTTP
runWebT :: forall m b. (Functor m, H.ToMessage b) => H.WebT m b ->
           m (Maybe H.Response)
runWebT = (fmap . fmap) appFilterToResp . H.ununWebT
  where
    appFilterToResp :: (Either H.Response b, H.FilterFun H.Response) ->
                       H.Response
    appFilterToResp (e, ff) = H.unFilterFun ff $ either id H.toResponse e

