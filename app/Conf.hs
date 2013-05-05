module Conf where

import qualified Model as M

import qualified Language.Soy as Soy

data StartupOption
  = StartupOption {
    opt_host :: String,
    opt_port :: Int,
    opt_mongoHost :: String,
    opt_mongoNeedAuth :: Bool
  }
  deriving (Show)

data SiteConf
  = SiteConf {
    sc_soyConf :: Soy.RenderConfig,
    sc_opt :: StartupOption,
    sc_dbInfo :: M.DBInfo
  }
