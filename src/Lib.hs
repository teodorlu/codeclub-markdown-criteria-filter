module Lib
    ( someFunc
    ) where

import Text.Pandoc.Walk
import Text.Pandoc.JSON
import Flow

import qualified Data.String as String

-- Beware, thy who enters.
-- What follows was written late at night, fueled upon many 'a sugary fluid.

someFunc :: IO ()
someFunc = toJSONFilter walkerTexasRanger
-- someFunc = putStrLn "someFunc"

walkerTexasRanger :: Pandoc -> Pandoc
walkerTexasRanger (Pandoc meta blocks) = Pandoc meta (update_guide_to_new_template blocks)

-- Step 1: encode the above text into Pandoc's internat format. Should be easy.
-- Use a dummy document that contains that text, and convert it to native.
filltext_encoded = [Para [Str "Det",Space,Str "er",Space,Str "mange",Space,Str "ulike",Space,Str "m\229ter",Space,Str "en",Space,Str "kan",Space,Str "vurdere",Space,Str "et",Space,Str "programmeringsprosjekt,",Space,Str "og",Space,Str "her",Space,Str "m\229",Space,Str "en",SoftBreak,Str "selv",Space,Str "vurdere",Space,Str "hva",Space,Str "som",Space,Str "er",Space,Str "den",Space,Str "beste",Space,Str "m\229ten",Space,Str "ut",Space,Str "ifra",Space,Str "hvilket",Space,Str "fag",Space,Str "man",Space,Str "jobber",Space,Str "i,",SoftBreak,Str "hvilken",Space,Str "aldergruppe",Space,Str "og",Space,Str "hvilket",Space,Str "niv\229",Space,Str "elevene",Space,Str "er",Space,Str "p\229,",Space,Str "hva",Space,Str "man",Space,Str "\248nsker",Space,Str "\229",Space,Str "teste",Space,Str "og",SoftBreak,Str "hvor",Space,Str "mye",Space,Str "tid",Space,Str "man",Space,Str "har",Space,Str "til",Space,Str "r\229dighet",Space,Str "til",Space,Str "\229",Space,Str "jobbe",Space,Str "med",Space,Str "prosjektet.",Space,Str "I",SoftBreak,Str "v\229rt",Space,Link ("",[],[]) [Str "l\230rerdokument"] ("../../pages/hvordan_bruke_l\230rerveiledning.html",""),Space,Str "har",Space,Str "vi",SoftBreak,Str "blant",Space,Str "annet",Space,Str "beskrevet",Space,Str "ulike",Space,Str "m\229ter",Space,Str "dette",Space,Str "kan",Space,Str "gj\248res",Space,Str "p\229,",Space,Str "tillegg",Space,Str "til",Space,Str "en",Space,Str "del",Space,Str "andre",SoftBreak,Str "nyttige",Space,Str "tips",Space,Str "til",Space,Str "hvordan",Space,Str "man",Space,Str "underviser",Space,Str "i",Space,Str "programmering."]]

filltext_small = [Para [Str "YOLO"]]

-- (header_level, header_text for matches)
begin_header_match = [Str "Forslag",Space,Str "til",Space,Str "vurderingskriterier"]
end_header_match = [Str "Forutsetninger",Space,Str "og",Space,Str "utstyr"]

-- Seems to work. Commit!

-- Now, onto the real work. Need to do some kind of filter. First, try to just
-- replace the relevant headers with some NANANA

matches_header :: [Inline] -> Block -> Bool
matches_header spec (Header 2 attrs inlines) =
  inlines == spec
matches_header _ _ = False

sampleDocument = [Header 1 ("first-header",[],[]) [Str "First",Space,Str "header"]
                 ,Para [Str "Some",Space,Str "paragraph",Space,Str "in",Space,Str "between."]
                 ,Para [Str "This",Space,Str "doesn't",Space,Str "really",Space,Str "matter."]
                 ,Header 2 ("forslag-til-vurderingskriterier",["challenge"],[]) [Str "Forslag",Space,Str "til",Space,Str "vurderingskriterier"]
                 ,BulletList
                  [[Plain [Str "Item"]]
                  ,[Plain [Str "Another",Space,Str "item"]]]
                 ,CodeBlock ("",["python"],[]) "def f(x):\n    return x + 9"
                 ,Header 2 ("forutsetninger-og-utstyr",["challenge"],[]) [Str "Forutsetninger",Space,Str "og",Space,Str "utstyr"]
                 ,Header 1 ("third-item",[],[]) [Str "Third",Space,Str "item"]]

type Matcher = Block -> Bool

swap_between :: Matcher -> Matcher -> [Block] -> [Block] -> [Block]
swap_between is_begin is_end target (b:bs)
  = if is_begin b
    then [b] ++ target ++ strip_untill is_end bs
    else b : swap_between is_begin is_end target bs
swap_between _ _ _ [] = []

strip_untill is_end [] = []
strip_untill is_end (b:bs)
  = if is_end b
    then b:bs -- Keep the rest
    else strip_untill is_end bs -- Else strip!

-- Now I think we have what we need. Put it together!

nope = \_ -> False

update_guide_to_new_template
  = swap_between
      (matches_header begin_header_match) -- The beginning of the match
      (matches_header end_header_match) -- The end of the match
      filltext_encoded -- Replacing with the encoded filltext

runtests =
  update_guide_to_new_template sampleDocument |> showthings

showthings x =
  x
    |> map show
    |> unlines
    |> putStr
