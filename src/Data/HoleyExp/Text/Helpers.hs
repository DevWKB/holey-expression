{-|
Module      : Helpers
Description : Useful `Text` helper functions.
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

-}
module  Data.HoleyExp.Text.Helpers 
    (-- * Text Helpers
     between
    ,braces
    ,brackets
    ,prettyList
    ,doubleQuote
    ,prettyDouble
    ,parens
    ,quotes
    ,singleQuotes) where

import Data.Text          (Text)
import Data.Text          qualified as DT

-- | Add a prefix and a suffix to the input text.
between :: Text -> Text -> Text -> Text
between b a t = b <> t <> a

-- | Add braces around the input text.
braces :: Text -> Text
braces = between (DT.singleton '{') (DT.singleton '}')

-- | Add brackets around the input text.
brackets :: Text -> Text
brackets = between (DT.singleton '[') (DT.singleton ']')

-- | Add parens around the input text.
parens :: Text -> Text
parens = between (DT.singleton '(') (DT.singleton ')')

-- | Add double quotes around the input text.
quotes :: Text -> Text
quotes = between (DT.singleton '"') (DT.singleton '"')

-- | Add single quotes around the input text.
singleQuotes :: Text -> Text
singleQuotes = between (DT.singleton '\'') (DT.singleton '\'')

-- | Convert the input list into a comma separated list in a human-readable
-- format. This is essentially `Data.Text.show`, but without the quoting of
-- literals.
prettyList :: (a -> Text) -> [a] -> Text
prettyList f = brackets . aux 
    where
        aux []     = DT.Empty
        aux [x]    = f x
        aux (x:xs) = f x <> ", " <> aux xs

-- | Convert the input double into a human-readable format. This drops the
-- decimal point when the input is a whole number.
prettyDouble :: Double -> Text
prettyDouble (DT.show->n) =     
    case DT.break (=='.') n of
        (ds,".0") -> ds
        _ -> n

-- | Double quote the input text.
doubleQuote :: DT.Text -> DT.Text
doubleQuote = between (DT.singleton '\"') (DT.singleton '\"')
