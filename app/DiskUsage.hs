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
    when shouldLog $ logDiffTS st_field

recordEntry :: FilePath -> FileStatus -> BillingApp FileOffset ()
recordEntry filePath fileStatus = do
    ext <- asks ext
    when (needRecord filePath ext $ isRegularFile fileStatus) (addToTS $ fileSize fileStatus)
  where
    addToTS :: FileOffset -> BillingApp FileOffset ()
    addToTS ofs = modify (\st -> st {st_field = st_field st + ofs})
    needRecord _ Nothing _ = True
    needRecord fp (Just ext) isFile = isFile && (ext == takeExtension fp)

logDiffTS :: FileOffset -> BillingApp FileOffset ()
logDiffTS ts = do
    AppState {..} <- get
    tell [(currentPath, st_field - ts)]
