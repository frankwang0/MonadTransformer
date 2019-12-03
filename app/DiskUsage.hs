{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module DiskUsage (diskUsage) where

import Control.Monad.RWS
import System.FilePath (takeExtension)
import System.Posix.Types (FileOffset)
import System.PosixCompat.Files (FileStatus, getFileStatus,
                                 isDirectory, isRegularFile, fileSize)

import TraverseDir
import AppRWS

diskUsage :: BillingApp FileOffset ()
diskUsage = do
    maxDepth <- asks maxDepth
    AppState {..} <- get
    fileStatus <- liftIO $ getFileStatus currentPath
    let isDir = isDirectory fileStatus
        shouldLog = isDir && currentDepth <= maxDepth
    when isDir $ traverseDirectory diskUsage
    recordEntry currentPath fileStatus 
    when shouldLog $ logDiffTS size

recordEntry :: FilePath -> FileStatus -> BillingApp FileOffset ()
recordEntry filePath fileStatus = do
    ext <- asks ext
    when (shouldRecord filePath ext $ isRegularFile fileStatus) (addToTS $ fileSize fileStatus)
  where
    addToTS :: FileOffset -> BillingApp FileOffset ()
    addToTS offset = modify (\st -> st {size = size st + offset})
    shouldRecord _ Nothing _ = True
    shouldRecord fp (Just ext) isFile = isFile && (ext == takeExtension fp)

logDiffTS :: FileOffset -> BillingApp FileOffset ()
logDiffTS ts = do
    AppState {..} <- get
    tell [(currentPath, size - ts)]
