{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Control.Monad
import Data.Text.Lazy (LazyText)
import Options.Applicative
import System.Exit
import Text.Read

import qualified Data.Text.Lazy.IO as LIO
import qualified Days.Day as D
import qualified Days.Day0 as Day0

data Day
    = Day0
    | Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | Day10
    | Day11
    | Day12
    deriving (Eq, Enum)

parseDay :: ReadM Day
parseDay = eitherReader $ \s -> do
    d <- readEither s
    if d < 0 || d > 12
        then Left "error: day must be a number in the range [1-12]"
        else Right . toEnum $ d

data Part
    = BothParts
    | Part1
    | Part2
    deriving (Eq)

parsePart :: ReadM Part
parsePart = eitherReader $ \case
    "1" -> Right Part1
    "2" -> Right Part2
    _ -> Left "error: part must be a number in the range [1-2]"

data Args = Args
    { day :: Day
    , part :: Part
    , puzzleInput :: FilePath
    }

args :: Parser Args
args =
    Args
        <$> option
            parseDay
            ( short 'd'
                <> metavar "DAY"
                <> help "which day [0-12] to run"
            )
        <*> option
            parsePart
            ( short 'p'
                <> metavar "PART"
                <> value BothParts
                <> help "which part [1-2] to run, leave unspecified for both parts"
            )
        <*> strArgument
            ( metavar "INPUT_FILE"
                <> help "path to a file containing the puzzle input"
            )

main :: IO ()
main = runAoc =<< execParser opts
  where
    opts =
        info
            (args <**> helper)
            ( fullDesc
                <> header "This is the Advent of Code 2025"
            )

runAoc :: Args -> IO ()
runAoc (Args{day, part, puzzleInput}) = do
    input <- LIO.readFile puzzleInput
    case day of
        Day0 -> runPart @Day0.Day0 input part
        _ -> die "error: day not yet implemented"

runPart :: forall a part1 part2. (D.Day a part1 part2) => LazyText -> Part -> IO ()
runPart input p =
    when
        (p == BothParts || p == Part1)
        (print . D.part1 $ parsedInput)
        >> when
            (p == BothParts || p == Part2)
            (print . D.part2 $ parsedInput)
  where
    parsedInput :: a
    parsedInput = D.parseDay input
