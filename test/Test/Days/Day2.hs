module Test.Days.Day2 (testDay2) where

import Days.Day
import Days.Day2
import Test.HUnit
import Test.Util

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LIO

input :: Day2
input =
    Day2
        $ map
            T.pack
            [ "11"
            , "12"
            , "13"
            , "14"
            , "15"
            , "16"
            , "17"
            , "18"
            , "19"
            , "20"
            , "21"
            , "22"
            , "95"
            , "96"
            , "97"
            , "98"
            , "99"
            , "100"
            , "101"
            , "102"
            , "103"
            , "104"
            , "105"
            , "106"
            , "107"
            , "108"
            , "109"
            , "110"
            , "111"
            , "112"
            , "113"
            , "114"
            , "115"
            , "998"
            , "999"
            , "1000"
            , "1001"
            , "1002"
            , "1003"
            , "1004"
            , "1005"
            , "1006"
            , "1007"
            , "1008"
            , "1009"
            , "1010"
            , "1011"
            , "1012"
            , "1188511880"
            , "1188511881"
            , "1188511882"
            , "1188511883"
            , "1188511884"
            , "1188511885"
            , "1188511886"
            , "1188511887"
            , "1188511888"
            , "1188511889"
            , "1188511890"
            , "222220"
            , "222221"
            , "222222"
            , "222223"
            , "222224"
            , "1698522"
            , "1698523"
            , "1698524"
            , "1698525"
            , "1698526"
            , "1698527"
            , "1698528"
            , "446443"
            , "446444"
            , "446445"
            , "446446"
            , "446447"
            , "446448"
            , "446449"
            , "38593856"
            , "38593857"
            , "38593858"
            , "38593859"
            , "38593860"
            , "38593861"
            , "38593862"
            , "565653"
            , "565654"
            , "565655"
            , "565656"
            , "565657"
            , "565658"
            , "565659"
            , "824824821"
            , "824824822"
            , "824824823"
            , "824824824"
            , "824824825"
            , "824824826"
            , "824824827"
            , "2121212118"
            , "2121212119"
            , "2121212120"
            , "2121212121"
            , "2121212122"
            , "2121212123"
            , "2121212124"
            ]

testDecrement :: Assertion
testDecrement =
    assertDecrement "101" "100"
        >> assertDecrement "100000" "99999"
  where
    assertDecrement i expected = decrement (T.pack i) @?= T.pack expected

testExpandRangeOnto :: Assertion
testExpandRangeOnto = actual @?= expected
  where
    actual = expandRangeOnto (T.pack "11") (T.pack "22") []
    expected = map T.show [(11 :: Int) .. 22]

testPart1Sample :: Assertion
testPart1Sample = actual @?= expected
  where
    actual = part1 input
    expected = 1227775554

testPart1 :: Assertion
testPart1 = do
    inputFixture <- LIO.readFile "fixtures/day2-input.txt"
    let parsedInput = parseDay inputFixture :: Day2
    let actual = part1 parsedInput
    actual @?= 30608905813

testDay2 :: Test
testDay2 =
    "Day 2"
        ~: [ "decrement" ~: testDecrement
           , "expandRangeOnto" ~: testExpandRangeOnto
           , "parseDay" ~: testParseDay "fixtures/day2-sample-input.txt" input
           , "Part 1 Sample" ~: testPart1Sample
           , "Part 1" ~: testPart1
           ]
