module Days.Day2 (Day2 (..), decrement, expandRangeOnto) where

import Control.Exception (assert)
import Data.Char
import Data.Foldable
import Data.Function
import Data.Monoid
import Data.Text (StrictText)
import Days.Day
import Util.Parsing

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

newtype Day2 = Day2 [StrictText]
    deriving (Show, Eq)

data ParseState'
    = ParsingLower
    | ParsingUpper
    deriving (Eq)

data ParseState = ParseState
    { lowerId :: StrictText
    , upperId :: StrictText
    , which :: ParseState'
    , ids :: [StrictText]
    }

emptyParseState :: ParseState
emptyParseState =
    ParseState
        { lowerId = T.empty
        , upperId = T.empty
        , which = ParsingUpper
        , ids = []
        }

decrement :: StrictText -> StrictText
decrement =
    T.dropWhile (== '0')
        . fst
        . T.foldr
            ( \digit (d, carry) ->
                if carry
                    then case digit of
                        '0' -> ('9' `T.cons` d, True)
                        '1' -> ('0' `T.cons` d, False)
                        '2' -> ('1' `T.cons` d, False)
                        '3' -> ('2' `T.cons` d, False)
                        '4' -> ('3' `T.cons` d, False)
                        '5' -> ('4' `T.cons` d, False)
                        '6' -> ('5' `T.cons` d, False)
                        '7' -> ('6' `T.cons` d, False)
                        '8' -> ('7' `T.cons` d, False)
                        '9' -> ('8' `T.cons` d, False)
                        _ -> error "cannot decrement a non-numeric string"
                    else (digit `T.cons` d, False)
            )
            (T.empty, True)

expandRangeOnto :: StrictText -> StrictText -> [StrictText] -> [StrictText]
expandRangeOnto lower upper expanded
    | lower == upper = upper : expanded
    | otherwise = expandRangeOnto lower (decrement upper) (upper : expanded)

sumInvalidIds :: (StrictText -> Bool) -> [StrictText] -> Int
sumInvalidIds isInvalidId = getSum . foldMap' (Sum . parseInt') . filter isInvalidId

instance Day Day2 Int Int where
    parseDay =
        Day2
            . ids
            . L.foldr
                ( \c parseState@ParseState{lowerId, upperId, which, ids} -> case c of
                    '-' -> parseState{which = assert (which == ParsingUpper) ParsingLower}
                    ',' ->
                        ParseState
                            { lowerId = T.empty
                            , upperId = T.empty
                            , which = assert (which == ParsingLower) ParsingUpper
                            , ids = expandRangeOnto lowerId upperId ids
                            }
                    whitespace | isSpace whitespace -> parseState -- ignore whitespace
                    digit | which == ParsingLower -> assert (isDigit digit) (parseState{lowerId = digit `T.cons` lowerId})
                    digit -> assert (isDigit digit) (parseState{upperId = digit `T.cons` upperId})
                )
                emptyParseState
            . (',' `L.cons`)

    part1 (Day2 ids) = sumInvalidIds isInvalidId ids
      where
        isInvalidId i =
            let l = T.length i `div` 2
             in T.take l i == T.drop l i

    part2 (Day2 ids) = sumInvalidIds isInvalidId ids
      where
        isInvalidId i =
            any
                ( \prefix ->
                    let n = (div `on` T.length) i prefix
                     in n > 1 && T.replicate n prefix == i
                )
                . dropWhile T.null
                . T.inits
                $ i
