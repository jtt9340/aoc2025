module Main (main) where

-- Ripped straight from https://github.com/commercialhaskell/stack-templates/blob/master/rubik.hsfiles#L102-L118
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    hints <- hlint ["app", "src", "test"]
    if null hints then exitSuccess else exitFailure
