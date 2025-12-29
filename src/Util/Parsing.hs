module Util.Parsing (parseInt, parseInt') where

import Data.Text (StrictText)
import Data.Text.Lazy (LazyText)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Read as LR
import qualified Data.Text.Read as R

parseInt :: (Integral a) => LazyText -> a
parseInt text = case LR.decimal text of
    Left e -> error e
    Right (_, t) | not $ L.null t -> error $ L.unpack text ++ " is not a number"
    Right (i, _) -> i

parseInt' :: (Integral a) => StrictText -> a
parseInt' text = case R.decimal text of
    Left e -> error e
    Right (_, t) | not $ T.null t -> error $ T.unpack text ++ " is not a number"
    Right (i, _) -> i
