module Days.Day2 (Day2 (..)) where

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

sumInvalidIds :: (StrictText -> Bool) -> [StrictText] -> Int
sumInvalidIds isInvalidId = getSum . foldMap' (Sum . parseInt') . filter isInvalidId

instance Day Day2 Int Int where
    parseDay =
        Day2
            . concatMap
                ( \range ->
                    let [lo, hi] = T.split (== '-') . T.stripEnd . L.toStrict $ range
                     in expandRange lo hi
                )
            . L.split (== ',')
      where
        expandRange lo hi = expandRange' lo hi []

        expandRange' lo hi expanded
            | lo == hi = hi : expanded
            | otherwise = expandRange' lo (decrement hi) (hi : expanded)

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
