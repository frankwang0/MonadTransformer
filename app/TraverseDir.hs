{-# LANGUAGE RecordWildCards #-}

module TraverseDir (traverseDirectory) where

import Data.Foldable (traverse_)
import Control.Monad.RWS
import System.Directory (listDirectory)
import System.FilePath ((</>))

import AppRWS

traverseDirectory :: BillingApp s () -> BillingApp s ()
traverseDirectory app = do
    path <- gets currentPath
    content <- liftIO $ listDirectory path
    traverse_ (go path) content
  where
    go path name = do
      modify (newPath $ path </> name)
      app
      modify (restorePath $ path)
      
    newPath path st @ AppState {..} = st {currentDepth = currentDepth + 1,
                                          currentPath = path}
    restorePath path st @ AppState {..} = st {currentDepth = currentDepth - 1,
                                              currentPath = path}