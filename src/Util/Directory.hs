module Util.Directory (findHaskellSources) where

import Control.Monad
import System.Directory

import qualified System.FilePath as FP

findHaskellSources :: FilePath -> (FilePath -> IO ()) -> IO ()
findHaskellSources path action
    | FP.takeFileName path == ".stack-work" = return ()
    | otherwise = mapM_ (go . (path FP.</>)) =<< listDirectory path
  where
    go p = do
        isDir <- doesDirectoryExist p
        if isDir
            then findHaskellSources p action
            else when (FP.takeExtension p == ".hs" && FP.takeBaseName p /= "Setup") (action p)
