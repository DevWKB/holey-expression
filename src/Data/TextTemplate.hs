{-|
Module      : Template
Description : Framework for creating text templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.me

Framework for creating text templates. These are text with holes that can
be filled and plugged. No parsing of the actual text is done, but the text
is broken up into `chunk`'s in between the `hole`'s. Then a fill or plug
function can be defined to replace the holes with text; see `fillHole` and
`plugHole`. 

The recommended way to programmatically generate templates is to use the
quasi-quoter which makes writing templates easier. If you don't want to depend
on Template Haskell, then we strongly recommend to use the template combinators,
because they keep track of internal state to make certain operations on
templates efficient.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.TextTemplate (-- * Text Templates
                           bracketTemplate
                          ,braceTemplate
                          -- ** Parsing
                          ,Parser
                          ,TParseError
                          ,templateParser
                          ,parseTemplate
                          ,varParser                          
                          -- *** Helpers
                          ,maybeParser
                          ,doubleQuotedParser
                          ,runParsecT) where

import Data.TextTemplate.TemplateInternal (Template,HoleFillingExp(..),ToTemplate,chunk,pattern Chunk,pattern Compose,betweenTemplate)
import Text.Megaparsec (ShowErrorComponent (..), ParseErrorBundle, ParsecT, Parsec, MonadParsec (..), parse, errorBundlePretty, runParserT, many, choice, satisfy, customFailure, some, (<|>), atEnd, skipCount, between)
import Data.Text (Text)
import Data.Void (Void)
import Data.NatMap (Natural)
import Data.String (IsString (..))
import qualified Data.Text as DT
import Text.Megaparsec.Char (string,digitChar, char, space)
import Data.Char (isAsciiLower, isAlphaNum, isAscii)
import Data.Maybe (isNothing)
import qualified Data.List as L
import Text.Megaparsec.Byte.Lexer (symbol)

runParsec :: ShowErrorComponent e => Parsec e Text b -> Text -> Either Text b
runParsec p s = case parse p "text-templates" s of
    Left bundle -> Left . DT.pack $ errorBundlePretty bundle
    Right t -> Right t

runParsecT 
    :: (Monad m, ShowErrorComponent e) 
    => (m (Either (ParseErrorBundle Tok e) a) -> (Either (ParseErrorBundle Tok e) a))
    -> ParsecT e Text m a
    -> Text
    -> Either Text a
runParsecT eval p s = 
    case eval (runParserT p "" s) of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right t -> Right t
        
instance HoleFillingExp Text Text where
    hfExpToText :: Text -> Text
    hfExpToText = id

    parseHFExp :: Maybe (Text -> Either Text Text)
    parseHFExp = Just $ runParsec textFillingParser
        where            
            textFillingParser = DT.pack <$> many charTextFillingParser

            charTextFillingParser :: Parsec TParseError Tok Char
            charTextFillingParser = choice [
                    satisfy (\c -> c /= '{' && c /= '}' && c /= '\\'),
                    escapeCharTextFillingParser
                ]

            escapeCharTextFillingParser :: Parsec TParseError Tok Char
            escapeCharTextFillingParser = do
                skip (string "\\")
                satisfy (`elem` ['{','}','\\'])

instance HoleFillingExp Text Int where
  hfExpToText :: Int -> Text
  hfExpToText = DT.show
  
  parseHFExp :: Maybe (Text -> Either Text Int)
  parseHFExp = Just . runParsec @Void $ read <$> many digitChar

varParser :: Parser String
varParser = do
    -- Make sure we start with a lower-case ascii letter.
    c <- maybeParser . lookAhead $ takeWhile1P Nothing isAsciiLower
    if isNothing c
    then customFailure $ HFExpParseError "variables must being with a lower-case letter"
    else DT.unpack <$> takeWhile1P Nothing (\c -> isAlphaNum c && isAscii c)

-- | Add brackets `[]` around the input template.
bracketTemplate :: (ToTemplate Text filling a) => a -> Template Text filling
bracketTemplate = betweenTemplate (chunk "[") (chunk "]")

-- | Add braces `{}` around the input template.
braceTemplate :: (ToTemplate Text filling a) => a -> Template Text filling
braceTemplate = betweenTemplate (chunk "{") (chunk "}")

-- | Parse a template.
parseTemplate :: HoleFillingExp Text filling => Text -> Either Text (Template Text filling)
parseTemplate s = 
    case parse templateParser "text-templates" s of
         Left bundle -> Left . DT.pack $ errorBundlePretty bundle
         Right t -> Right t

-- | Parse errors

data TParseError
    = HFExpParseError Text
    deriving (Eq,Ord,Show)

instance ShowErrorComponent TParseError where
    showErrorComponent :: TParseError -> String
    showErrorComponent err = "text-templates-parser: " <> showErrorComponent' err
        where
            showErrorComponent' (HFExpParseError err) = DT.unpack err

-- | Type of tokens.
type Tok    = Text
-- | Type of the parsers that operate on a stream of `Tok`.
type Parser = Parsec TParseError Tok 

-- | Parse a hole index (`Natural`).
holeIndexParser :: Parser Natural
holeIndexParser = do
    ds <- some digitChar
    pure . read $ ds

maybeParser :: MonadParsec e s f => f a -> f (Maybe a)
maybeParser p = try (Just <$> p) <|> pure Nothing

-- | Parse a hole's filling which must be escaped properly.
holeFillingParser :: HoleFillingExp Text filling => Parser (Maybe filling)
holeFillingParser = maybe n p (parseHFExp @Text)
    where
        -- If there is no filling, then skip the braces.
        n = (skip $ string "{}") >> pure Nothing

        p :: (Text -> Either Text filling) -> Parser (Maybe filling)
        p expParser = do
            f <- Text.Megaparsec.between (char '{') (char '}') $ many $ templateCharParser True
            if L.null f
            then pure Nothing 
            else do let e = expParser . DT.pack $ f
                    case e of
                        Left err -> customFailure $ HFExpParseError err
                        Right f' -> pure . Just $ f'

-- | Parse a `Hole`. That is, a pair of a hole index and a filling.
holeParser :: HoleFillingExp Text filling => Parser (Natural, Maybe filling)
holeParser = do
    skip (string "$")
    i <- holeIndexParser
    f <- holeFillingParser
    pure $ (i, f)

-- | Parse a `Chunk`.
chunkParser :: IsString text => Parser text
chunkParser = fromString <$> many (templateCharParser False)

-- | Parse a template either as a `Chunk` or a `Compose`.
templateParser :: HoleFillingExp Text filling => Parser (Template Text filling)
templateParser = do
    mc <- chunkParser
    isEnd <- atEnd
    if isEnd
    then pure . Chunk $ mc
    else do h <- holeParser
            t <- templateParser
            pure $ Compose mc h t

-- | Parse a template character. These are any unicode character where the
-- characters @['$','{','}','\\']@ are escaped when parsing a hole's filling,
-- otherwise just @'$'@ needs to be escaped.
templateCharParser :: Bool -> Parser Char
templateCharParser filling = choice [
        satisfy (\c -> c /= '$' && c /= '\'' && (if filling then c /= '{' && c /= '}' else True) && c /= '\\'),
        escapedTemplateCharParser
    ]

-- | Parsed an escaped character; one of, @["\\$"","\\{"","\\}","\\\\"]@.
escapedTemplateCharParser :: Parser Char
escapedTemplateCharParser = do
    skipCount 1 (char '\\')
    satisfy (\c -> c == '$' || c == '{' || c == '}' || c == '\'')

-- * Helper parsers

-- | Parse a double-quoted output of the input parser.
doubleQuotedParser :: Ord e => Parsec e Tok a -> Parsec e Tok a
doubleQuotedParser = Text.Megaparsec.between (string "\"") (tok "\"")

-- * Tokens

-- | Parse a token (unicode character)
-- Consumes whitespace *after* the parsed token.
tok :: Ord e => Tok -> Parsec e Tok Tok
tok = symbol space

-- | Parse and throw away the symbol parsed by the input token
skip :: Parsec e Tok Tok -> Parsec e Tok ()
skip = skipCount 1

