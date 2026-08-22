{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Text
Description : Holey Text Expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

-}
module Data.HoleyExp.Text
(-- * Templates
     Data.Text.Text
    ,module Data.HoleyExp.HExp
    -- * Text Templates
    ,bracketTemplate
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
    ,runParsecT
     -- * Text Helpers
    ,between
    ,braces
    ,brackets
    ,prettyList
    ,doubleQuote
    ,prettyDouble) where

import Data.Text (Text)
import Data.Text qualified as DT
import Text.Megaparsec (ShowErrorComponent (..), Parsec, ParseErrorBundle, ParsecT, MonadParsec (..), parse, errorBundlePretty, runParserT, many, choice, satisfy, customFailure, some, (<|>), atEnd, skipCount)

import Data.HoleyExp.HExp
import Data.Void (Void)
import Data.NatMap (Natural)
import Data.String (IsString (fromString))
import Text.Megaparsec.Char (string, digitChar, char, space)
import Data.Char (isAsciiLower, isAlphaNum, isAscii)
import Data.Maybe (isNothing)
import qualified Text.Megaparsec as MT
import qualified Data.List as L
import Text.Megaparsec.Byte.Lexer ( symbol )

-- | Type of tokens.
type Tok = Text

-- | Combinator for running a `Parsec` parser with a `Text` input stream and
-- custom error messages.
runParsec :: ShowErrorComponent e => Parsec e Text b -> Text -> Either Text b
runParsec p s = case parse p "text-templates" s of
    Left bundle -> Left . DT.pack $ errorBundlePretty bundle
    Right t -> Right t

-- | Combinator for running a `ParsecT` parser with a `Text` input stream and
-- custom error messages.
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

instance TextLike Double where
    toText :: Double -> Text
    toText = DT.show

instance HoleFillingExp Text Double where
    hfExpToText :: Double -> Text
    hfExpToText = toText

    parseHFExp :: Maybe (Text -> Either Text Double)
    parseHFExp = Just . runParsec @Void $ p
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

-- | Add brackets `[]` around the input template.
bracketTemplate :: (ToHExp Text filling a) => a -> HExp Text filling
bracketTemplate = betweenHExp (chunk "[") (chunk "]")

-- | Add braces `{}` around the input template.
braceTemplate :: (ToHExp Text filling a) => a -> HExp Text filling
braceTemplate = betweenHExp (chunk "{") (chunk "}")

-- | Parse a template.
parseTemplate :: HoleFillingExp Text filling => Text -> Either Text (HExp Text filling)
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

-- | Type of the parsers that operate on a stream of t`Text`.
type Parser = Parsec TParseError Tok 

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
holeFillingParser :: HoleFillingExp Text filling => Parser (Maybe filling)
holeFillingParser = maybe n p (parseHFExp @Text)
    where
        -- If there is no filling, then skip the braces.
        n = (skip $ string "{}") >> pure Nothing

        p :: (Text -> Either Text filling) -> Parser (Maybe filling)
        p expParser = do
            f <- MT.between (char '{') (char '}') $ many $ templateCharParser True
            if L.null f
            then pure Nothing 
            else do let e = expParser . DT.pack $ f
                    case e of
                        Left err -> customFailure $ HFExpParseError err
                        Right f' -> pure . Just $ f'

-- | Parse a `Data.HExp.Hole`. That is, a pair of a hole index and a filling.
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
templateParser :: HoleFillingExp Text filling => Parser (HExp Text filling)
templateParser = do
    mc <- chunkParser
    isEnd <- atEnd
    if isEnd
    then pure . Chunk $ mc
    else do h <- holeParser
            t <- templateParser
            pure $ Compose mc h t

-- | Parse a template character. These are any unicode character where the
-- characters 
-- > ["$","{","}","\\"] 
-- are escaped when parsing a hole's filling,
-- otherwise just @'$'@ needs to be escaped.
templateCharParser :: Bool -> Parser Char
templateCharParser filling = choice [
        satisfy (\c -> c /= '$' && c /= '\'' && (if filling then c /= '{' && c /= '}' else True) && c /= '\\'),
        escapedTemplateCharParser
    ]

-- | Parsed an escaped character; one of, 
-- > ["\\$"","\\{"","\\}","\\\\"]
-- .
escapedTemplateCharParser :: Parser Char
escapedTemplateCharParser = do
    skipCount 1 (char '\\')
    satisfy (\c -> c == '$' || c == '{' || c == '}' || c == '\'')

-- * Helper parsers

-- | Parse a double-quoted output of the input parser.
doubleQuotedParser :: Ord e => Parsec e Tok a -> Parsec e Tok a
doubleQuotedParser = MT.between (string "\"") (tok "\"")

-- * Tokens

-- | Parse a token (unicode character)
-- Consumes whitespace *after* the parsed token.
tok :: Ord e => Tok -> Parsec e Tok Tok
tok = symbol space

-- | Parse and throw away the symbol parsed by the input token
skip :: Parsec e Tok Tok -> Parsec e Tok ()
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