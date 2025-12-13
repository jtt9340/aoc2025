module Util.Parsing (parseInt) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Read as LR

parseInt :: (Integral a) => L.Text -> a
parseInt text = case LR.decimal text of
    Left e -> error e
    Right (_, t) | not $ L.null t -> error $ L.unpack text ++ " is not a number"
    Right (i, _) -> i
