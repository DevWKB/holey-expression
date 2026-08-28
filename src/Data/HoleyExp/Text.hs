{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Text
Description : Holey Expressions in Text
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

This is the library for working with holey expressions in "Data.Text". 

If you are new to this library, it is recommended to first read over the start
of the base module "Data.HoleyExp.HExp" for an introduction to how holey
expressions work. 

Here we give a number of example holey-expressions.

A simple example:

>>> let t = (chunk "Today's Temperature: ") +> (hole 1) +> (chunk " high/") +> (hole 2) +> (chunk " low") :: HExp Text Double
>>> t
Today's Temperature: $1{} high/$2{} low

>>> plugAll t $ \_ -> \i -> if i == 1 then Just 91.2 else if i == 2 then Just 87.0 else Nothing 
Just "Today's Temperature: 91.2 high/87.0 low"

The above is an example of an expression of type @HExp Text Double@ where
the first type is the type of constants which is what we are ultimately constructing a value of when all
holes are plugged, and the second type is the type of the filling we place in
the holes.

A second way we can write the same expression using @OverloadedStrings@ is:

>>> let t'' = "Today's Temperature: " <> (hole 1) <> " high/" <> (hole 2) <> " low" :: HExp Text Double
>>> t ==> t''
True

We can also add a filling to holes in an expression:

>>> "Today's Temperature: " <> (filled 1 92.2) <> " high/" <> (filled 2 91.2) <> " low" :: HExp Text Double
Today's Temperature: $1{92.2} high/$2{91.2} low

-}
module Data.HoleyExp.Text
(-- * Holey Expressions     
    -- | This module reexports the holey-expression base.
    module Data.HoleyExp.HExp
    -- * Text Combinators
    ,Data.Text.Text
    ,bracketHExp
    ,braceHExp
    -- ** Parsing
    ,Parser
    ,TParseError
    ,hExpParser
    ,parseHExp
    ,varParser                          
    -- *** Helpers
    ,maybeParser
    ,doubleQuotedParser
    ,runParsecT
     -- * Text Helpers
    ,between
    ,braces
    ,brackets
    ,prettyList
    ,doubleQuote
    ,prettyDouble) where

import Data.HoleyExp.HExp

import Data.Text                  (Text)
import Data.Text                  qualified as DT
import Data.Void                  (Void)
import Data.NatMap                (Natural)
import Data.String                (IsString (fromString))
import Data.Char                  (isAsciiLower
                                  ,isAlphaNum
                                  ,isAscii)
import Data.Maybe                 (isNothing)
import Text.Megaparsec            (ShowErrorComponent (..)
                                  ,Parsec
                                  ,ParseErrorBundle
                                  ,ParsecT
                                  ,MonadParsec (..)
                                  ,parse
                                  ,errorBundlePretty
                                  ,runParserT
                                  ,many
                                  ,choice
                                  ,satisfy
                                  ,customFailure
                                  ,some
                                  ,(<|>)
                                  ,atEnd
                                  ,skipCount)
import Text.Megaparsec.Char       (string
                                  ,digitChar
                                  ,char
                                  ,space)
import Text.Megaparsec.Byte.Lexer (symbol)
import Text.Megaparsec            qualified as MT
import Data.List                  qualified as L

-- | Combinator for running a `Parsec` parser with a `Text` input stream and
-- custom error messages.
runParsec :: ShowErrorComponent e => Parsec e Text b -> Text -> Either Text b
runParsec p s = case parse p "holey-expression" s of
    Left bundle -> Left . DT.pack $ errorBundlePretty bundle
    Right t -> Right t

-- | Combinator for running a `ParsecT` parser with a `Text` input stream and
-- custom error messages.
runParsecT 
    :: (Monad m, ShowErrorComponent e) 
    => (m (Either (ParseErrorBundle Text e) a) -> (Either (ParseErrorBundle Text e) a))
    -> ParsecT e Text m a
    -> Text
    -> Either Text a
runParsecT eval p s = 
    case eval (runParserT p "" s) of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right t -> Right t
        
instance HoleFilling Text Text where
    fillingToText :: Text -> Text
    fillingToText = id

    parseFilling :: Maybe (Text -> Either Text Text)
    parseFilling = Just $ runParsec textFillingParser
        where            
            textFillingParser = DT.pack <$> many charTextFillingParser

            charTextFillingParser :: Parsec TParseError Text Char
            charTextFillingParser = choice [
                    satisfy (\c -> c /= '{' && c /= '}' && c /= '\\'),
                    escapeCharTextFillingParser
                ]

            escapeCharTextFillingParser :: Parsec TParseError Text Char
            escapeCharTextFillingParser = do
                skip (string "\\")
                satisfy (`elem` ['{','}','\\'])

instance HoleFilling Text Int where
  fillingToText :: Int -> Text
  fillingToText = DT.show
  
  parseFilling :: Maybe (Text -> Either Text Int)
  parseFilling = Just . runParsec @Void $ read <$> many digitChar

instance HoleFilling Text Double where
    fillingToText :: Double -> Text
    fillingToText = toText

    parseFilling :: Maybe (Text -> Either Text Double)
    parseFilling = Just . runParsec @Void $ p
        where
            p :: Parsec Void Text Double
            p = do d1 <- many digitChar 
                   c <- string "." >>= pure . DT.unpack
                   d2 <- many digitChar 
                   pure . read $ d1 <> c <> d2

-- | Parses a variable as a string. Variables must begin with a lower-case ascii
-- letter, and then contain ascii alpha-numeric characters.
varParser :: Parser String
varParser = do
    -- Make sure we start with a lower-case ascii letter.
    c <- maybeParser . lookAhead $ takeWhile1P Nothing isAsciiLower
    if isNothing c
    then customFailure $ HFExpParseError "variables must begin with a lower-case letter"
    else DT.unpack <$> takeWhile1P Nothing (\c -> isAlphaNum c && isAscii c)

-- | Add brackets `[]` around the input expressions.
bracketHExp :: (ToHExp Text filling a) => a -> HExp Text filling
bracketHExp = betweenHExp (chunk "[") (chunk "]")

-- | Add braces `{}` around the input expressions.
braceHExp :: (ToHExp Text filling a) => a -> HExp Text filling
braceHExp = betweenHExp (chunk "{") (chunk "}")

-- | Parse a holey expression in t`Text`.
parseHExp :: HoleFilling Text filling => Text -> Either Text (HExp Text filling)
parseHExp s = 
    case parse hExpParser "holey-expression" s of
         Left bundle -> Left . DT.pack $ errorBundlePretty bundle
         Right t -> Right t

-- | Parse errors

data TParseError
    = HFExpParseError Text
    deriving (Eq,Ord,Show)

instance ShowErrorComponent TParseError where
    showErrorComponent :: TParseError -> String
    showErrorComponent err = "holy-expression-parser: " <> showErrorComponent' err
        where
            showErrorComponent' (HFExpParseError err) = DT.unpack err

-- | Type of the parsers that operate on a stream of t`Text`.
type Parser = Parsec TParseError Text 

-- | Parse a hole index (`Natural`).
holeIndexParser :: Parser Natural
holeIndexParser = do
    ds <- some digitChar
    pure . read $ ds

-- | Parser combinator that attempts to parse using the input parser, and if it
-- fails, returns @Nothing@.
maybeParser :: MonadParsec e s f => f a -> f (Maybe a)
maybeParser p = try (Just <$> p) <|> pure Nothing

-- | Parse a hole's filling which must be escaped properly.
holeFillingParser :: HoleFilling Text filling => Parser (Maybe filling)
holeFillingParser = maybe n p (parseFilling @Text)
    where
        -- If there is no filling, then skip the braces.
        n = (skip $ string "{}") >> pure Nothing

        p :: (Text -> Either Text filling) -> Parser (Maybe filling)
        p expParser = do
            f <- MT.between (char '{') (char '}') $ many $ hExpCharParser True
            if L.null f
            then pure Nothing 
            else do let e = expParser . DT.pack $ f
                    case e of
                        Left err -> customFailure $ HFExpParseError err
                        Right f' -> pure . Just $ f'

-- | Parse a `Data.HExp.Hole`. That is, a pair of a hole index and a filling.
holeParser :: HoleFilling Text filling => Parser (Natural, Maybe filling)
holeParser = do
    skip (string "$")
    i <- holeIndexParser
    f <- holeFillingParser
    pure $ (i, f)

-- | Parse a `Chunk`.
chunkParser :: IsString text => Parser text
chunkParser = fromString <$> many (hExpCharParser False)

-- | Parse an expression either as a `Chunk` or a `Compose`.
hExpParser :: HoleFilling Text filling => Parser (HExp Text filling)
hExpParser = do
    mc <- chunkParser
    isEnd <- atEnd
    if isEnd
    then pure . Chunk $ mc
    else do h <- holeParser
            t <- hExpParser
            pure $ Compose mc h t

-- | Parse an expression character. These are any unicode character where the
-- characters 
-- > ["$","{","}","\\"] 
-- are escaped when parsing a hole's filling,
-- otherwise just @'$'@ needs to be escaped.
hExpCharParser :: Bool -> Parser Char
hExpCharParser filling = choice [
        satisfy (\c -> c /= '$' && c /= '\'' && (if filling then c /= '{' && c /= '}' else True) && c /= '\\'),
        escapedHExpCharParser
    ]

-- | Parsed an escaped character; one of, 
-- > ["\\$"","\\{"","\\}","\\\\"]
-- .
escapedHExpCharParser :: Parser Char
escapedHExpCharParser = do
    skipCount 1 (char '\\')
    satisfy (\c -> c == '$' || c == '{' || c == '}' || c == '\'')

-- * Helper parsers

-- | Parse a double-quoted output of the input parser.
doubleQuotedParser :: Ord e => Parsec e Text a -> Parsec e Text a
doubleQuotedParser = MT.between (string "\"") (tok "\"")

-- * Textens

-- | Parse a Texten (unicode character)
-- Consumes whitespace *after* the parsed Texten.
tok :: Ord e => Text -> Parsec e Text Text
tok = symbol space

-- | Parse and throw away the symbol parsed by the input Texten
skip :: Parsec e Text Text -> Parsec e Text ()
skip = skipCount 1

-- | Add a prefix and a suffix to the input text.
between :: Text -> Text -> Text -> Text
between b a t = b <> t <> a

-- | Add braces around the input text.
braces :: Text -> Text
braces = between (DT.singleton '{') (DT.singleton '}')

-- | Add brackets around the input text.
brackets :: Text -> Text
brackets = between (DT.singleton '[') (DT.singleton ']')

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
