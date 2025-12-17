module Days.Day2 (Day2 (..)) where

import Control.Exception (assert)
import Data.Char
import Days.Day
import Data.Text (StrictText)

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
emptyParseState = ParseState
    { lowerId = T.empty
    , upperId = T.empty
    , which = ParsingLower
    , ids = []
    }

decrement :: StrictText -> StrictText
decrement = fst . T.foldr
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
            else (digit, False) 
    )
    (T.empty, True)

expandRange :: StrictText -> StrictText -> [StrictText]
expandRange lower upper = expandRange' lower upper []
  where
    expandRange' lo hi expanded
        | lo == hi = expanded
        | otherwise = expandRange' lo (decrement hi) (hi : expanded)

instance Day Day2 Int Undefined where
    parseDay = Day2 . ids . L.foldr'
        ( \c parseState@ParseState{lowerId, upperId, which, ids} -> case c of
            '-' -> parseState { which = assert (which == ParsingUpper) ParsingLower }
            ',' -> ParseState
                    { lowerId = T.empty
                    , upperId = T.empty
                    , which = assert (which == ParsingLower) ParsingUpper
                    , ids = ids ++ expandRange lowerId UpperId
                    }
            digit | which == ParsingLower -> assert (isDigit digit) (parseState { lowerId = digit `T.cons` lowerId })
            digit -> assert (isDigit digit && which == ParsingUpper) (parseState { upperId = digit `T.cons` upperId })
        )
        emptyParseState

    part2 = undefined
