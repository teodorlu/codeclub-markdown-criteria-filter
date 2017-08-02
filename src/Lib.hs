module Lib
    ( someFunc
    ) where

import Text.Pandoc.Walk
import Text.Pandoc.JSON

someFunc :: IO ()
someFunc = toJSONFilter $ texasRanger
-- someFunc = putStrLn "someFunc"

texasRanger :: Pandoc -> Pandoc
texasRanger (Pandoc meta blocks) = Pandoc meta (map handleblock blocks)

handleblock :: Block -> Block
handleblock (Header 1 attrs inlines) = Header 2 attrs (capitalizeInlines inlines)
handleblock nonlevel1header = nonlevel1header

capitalizeInlines inlines = inlines

-- Now, let's see if we can unwrap that Pandoc to find some headers.
