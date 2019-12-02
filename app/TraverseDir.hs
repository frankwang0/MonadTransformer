{-# LANGUAGE RecordWildCards #-}

module TraverseDir (traverseDirectory) where

import Data.Foldable (traverse_)
import Control.Monad.RWS
import System.Directory (listDirectory)
import System.FilePath ((</>))

import AppRWS

traverseDirectory :: MyApp s () -> MyApp s ()
traverseDirectory app = do
    path <- gets curPath
    content <- liftIO $ listDirectory path
    traverse_ (go path) content
  where
    go path name = do
      modify (newPath $ path </> name)
      app
      modify (restorePath $ path)
      
    newPath path st @ AppState {..} = st {curDepth = curDepth + 1,
                                          curPath = path}
    restorePath path st @ AppState {..} = st {curDepth = curDepth - 1,
                                              curPath = path}

