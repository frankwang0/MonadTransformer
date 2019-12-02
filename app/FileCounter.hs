{-# LANGUAGE RecordWildCards #-}

module FileCounter (countFile) where

import Control.Monad.RWS
import System.FilePath
import System.Directory
import System.PosixCompat.Files

import TraverseDir
import AppRWS

countFile :: BillingApp Int ()
countFile = do
    AppState {..} <- get
    fs <- liftIO $ getFileStatus currentPath
    when (isDirectory fs) $ do
      AppConfig {..} <- ask
      when (currentDepth <= maxDepth) $ traverseDirectory countFile
      files <- liftIO $ listDirectory currentPath
      tell [(currentPath, length $ filterFiles ext files)]
  where
    filterFiles Nothing = id
    filterFiles (Just ext) = filter ((ext==).takeExtension)
