import Control.Monad.Reader
import Happstack.Server

import App

main = do
  option <- parseStartupOption
  siteConf <- makeSiteConf option
  let serverConf = nullConf {
        port = opt_port option
      }
      hostIp = opt_host option
      portNum = opt_port option
  sock <- bindIPv4 hostIp portNum
  putStrLn $ "Server started at " ++ show (hostIp, portNum)
  simpleHTTPWithSocket sock serverConf (runReaderT routes siteConf)

