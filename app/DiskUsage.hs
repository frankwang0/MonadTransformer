{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module DiskUsage (diskUsage) where

import Control.Monad.RWS
import System.FilePath (takeExtension)
import System.Posix.Types (FileOffset)
import System.PosixCompat.Files (FileStatus, getFileStatus,
                                 isDirectory, isRegularFile, fileSize)

import TraverseDir
import AppRWS

type DiskUsageApp = MyApp FileOffset

diskUsage :: DiskUsageApp ()
diskUsage = do
    maxDepth <- asks maxDepth
    AppState {..} <- get
    fs <- liftIO $ getFileStatus curPath
    let isDir = isDirectory fs
        shouldLog = isDir && curDepth <= maxDepth
    when isDir $ traverseDirectory diskUsage
    recordEntry curPath fs 
    when shouldLog $ logDiffTS st_field

recordEntry :: FilePath -> FileStatus -> DiskUsageApp ()
recordEntry fp fs = do
    ext <- asks ext
    when (needRec fp ext $ isRegularFile fs) (addToTS $ fileSize fs)
  where
    addToTS :: FileOffset -> DiskUsageApp ()
    addToTS ofs = modify (\st -> st {st_field = st_field st + ofs})
    needRec _ Nothing _ = True
    needRec fp (Just ext) isFile = isFile && (ext == takeExtension fp)

logDiffTS :: FileOffset -> DiskUsageApp ()
logDiffTS ts = do
    AppState {..} <- get
    tell [(curPath, st_field - ts)]
