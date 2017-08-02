module Lib
    ( someFunc
    ) where

import Text.Pandoc.Walk
import Text.Pandoc.JSON

someFunc :: IO ()
someFunc = toJSONFilter $ texasRanger
-- someFunc = putStrLn "someFunc"

texasRanger :: Pandoc -> Pandoc
texasRanger p = p
