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

{-
Now, let's see if we can do what we actually wanted.

Replace text like this:

## Forslag til vurderingskriterier {.challenge}

Something something darkside

## Forutsetninger og utstyr {.challenge}

To text like this:

## Forslag til vurderingskriterier {.challenge}

Det er mange ulike måter en kan vurdere et programmeringsprosjekt, og her må en
selv vurdere hva som er den beste måten ut ifra hvilket fag man jobber i,
hvilken aldergruppe og hvilket nivå elevene er på, hva man ønsker å teste og
hvor mye tid man har til rådighet til å jobbe med prosjektet. I vårt
[lærerdokument](../../pages/hvordan_bruke_lærerveiledning.html) har vi blant
annet beskrevet ulike måter dette kan gjøres på, tillegg til en del andre
nyttige tips til hvordan man underviser i programmering.

## Forutsetninger og utstyr {.challenge}

-}

-- Step 1: encode the above text into Pandoc's internat format. Should be easy.
-- Use a dummy document that contains that text, and convert it to native.
filltext_encoded = [Para [Str "Det",Space,Str "er",Space,Str "mange",Space,Str "ulike",Space,Str "m\229ter",Space,Str "en",Space,Str "kan",Space,Str "vurdere",Space,Str "et",Space,Str "programmeringsprosjekt,",Space,Str "og",Space,Str "her",Space,Str "m\229",Space,Str "en",SoftBreak,Str "selv",Space,Str "vurdere",Space,Str "hva",Space,Str "som",Space,Str "er",Space,Str "den",Space,Str "beste",Space,Str "m\229ten",Space,Str "ut",Space,Str "ifra",Space,Str "hvilket",Space,Str "fag",Space,Str "man",Space,Str "jobber",Space,Str "i,",SoftBreak,Str "hvilken",Space,Str "aldergruppe",Space,Str "og",Space,Str "hvilket",Space,Str "niv\229",Space,Str "elevene",Space,Str "er",Space,Str "p\229,",Space,Str "hva",Space,Str "man",Space,Str "\248nsker",Space,Str "\229",Space,Str "teste",Space,Str "og",SoftBreak,Str "hvor",Space,Str "mye",Space,Str "tid",Space,Str "man",Space,Str "har",Space,Str "til",Space,Str "r\229dighet",Space,Str "til",Space,Str "\229",Space,Str "jobbe",Space,Str "med",Space,Str "prosjektet.",Space,Str "I",SoftBreak,Str "v\229rt",Space,Link ("",[],[]) [Str "l\230rerdokument"] ("../../pages/hvordan_bruke_l\230rerveiledning.html",""),Space,Str "har",Space,Str "vi",SoftBreak,Str "blant",Space,Str "annet",Space,Str "beskrevet",Space,Str "ulike",Space,Str "m\229ter",Space,Str "dette",Space,Str "kan",Space,Str "gj\248res",Space,Str "p\229,",Space,Str "tillegg",Space,Str "til",Space,Str "en",Space,Str "del",Space,Str "andre",SoftBreak,Str "nyttige",Space,Str "tips",Space,Str "til",Space,Str "hvordan",Space,Str "man",Space,Str "underviser",Space,Str "i",Space,Str "programmering."]]

-- Seems to work. Commit!
