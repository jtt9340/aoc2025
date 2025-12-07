module Main (main) where

import Control.Monad
import Ormolu
import System.Directory
import System.Exit
import Util.Directory

import qualified Data.Text.IO.Utf8 as Text.IO

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    findHaskellSources cwd $ \p -> do
        source <- Text.IO.readFile p
        formatted <- ormoluFile defaultConfig p
        when (source /= formatted) (die $ "File is not formatted: " ++ p)
