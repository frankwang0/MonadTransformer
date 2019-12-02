module AppRWS where

import Control.Monad.RWS

data AppConfig = AppConfig {
      basePath :: FilePath,
      maxDepth :: Int,
      ext :: Maybe FilePath
    }

data AppState s = AppState {
      currentDepth :: Int,
      currentPath :: FilePath,
      st_field :: s
    }

type AppLog s = [(FilePath, s)]

type BillingApp s = RWST AppConfig (AppLog s) (AppState s) IO

runBillingApp :: BillingApp s a -> AppConfig -> s -> IO (a, AppLog s)
runBillingApp app config init =
  evalRWST app config (AppState 0 (basePath config) init)
