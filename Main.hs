import Control.Monad.Reader
import qualified Happstack.Server as H
import Happstack.Server.Wai as HW
import Network.Wai.Handler.Warp as W

import App

main = do
  option <- parseStartupOption
  siteConf <- makeSiteConf option
  let warpSetting = W.defaultSettings {
        settingsPort = portNum,
        settingsHost = W.Host hostIp
      }
      hostIp = opt_host option
      portNum = opt_port option

  sock <- H.bindIPv4 hostIp portNum
  putStrLn $ "Server started at " ++ show (hostIp, portNum)
  W.runSettingsSocket warpSetting sock $
    HW.toApplication (runReaderT routes siteConf)
  --simpleHTTPWithSocket sock serverConf (runReaderT routes siteConf)

