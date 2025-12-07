module Main (main) where

import Control.Monad
import Ormolu
import System.Directory
import Util.Directory

import qualified Data.Text.IO.Utf8 as Text.IO

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    findHaskellSources cwd $ \p -> do
        source <- Text.IO.readFile p
        formatted <- ormoluFile defaultConfig p
        when (source /= formatted) $ do
            putStr "Formatting "
            putStrLn p
            Text.IO.writeFile p formatted
