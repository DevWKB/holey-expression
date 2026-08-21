{-|
Module      : JSON Templates
Description : Text templates for JSON
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Text templates for JSON. The main use of this library is to test JSON
encoders/decoders, but there could be more use cases. This API is designed with
respect to [RFC 8259: STD 90: The JavaScript Object Notation (JSON) Data
Interchange Format](https://www.rfc-editor.org/info/rfc8259/).

Notes (need to explain):
- We do not allow duplicate keys.
- We require single quotes to be escaped due to templates.
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module  Data.TextTemplate.JSONInternal where

import Text.Megaparsec                    (Parsec
                                          ,ParseErrorBundle
                                          ,between
                                          ,skipCount
                                          ,many
                                          ,satisfy
                                          ,choice
                                          ,(<|>)
                                          ,errorBundlePretty
                                          ,sepBy1
                                          ,sepBy
                                          ,MonadParsec (..)
                                          ,customFailure
                                          ,ShowErrorComponent
                                          ,unexpected
                                          ,(<?>)
                                          ,ParsecT
                                          ,runParserT
                                          ,count
                                          ,takeWhileP)
import Data.Text                          (Text)
import Data.Text                          qualified as DT
import Data.Void                          (Void)
import Text.Megaparsec.Char               (string
                                          ,space
                                          ,space1)
import Data.Char                          (isPrint
                                          ,isHexDigit
                                          ,chr
                                          ,generalCategory
                                          ,isDigit
                                          ,isAsciiLower
                                          ,isAscii
                                          ,isAlphaNum)
import Text.Megaparsec.Char.Lexer         (float
                                          ,decimal
                                          ,symbol, signed)
import Language.Haskell.TH                qualified as TH
import Language.Haskell.TH.Quote          (QuasiQuoter(..))
import Language.Haskell.TH.Quote          qualified as TH
import Data.TextTemplate.TemplateInternal ((+>)
                                          ,chunk
                                          ,Template
                                          ,ToTemplate (..)
                                          ,HoleFillingExp, Proxy (runProxy)) 
import Data.TextTemplate.TemplateInternal qualified as StrT
import Data.TextTemplate.Text             qualified as StrT
import Data.TextTemplate.QQInternal       qualified as StrT
import Data.TextTemplate                  qualified as StrT
import Numeric                            (readHex)
import Text.Megaparsec.Error              (ShowErrorComponent(..)
                                          ,ErrorItem (..))
import Data.List.NonEmpty                 (fromList)
import Control.Monad.State                (State
                                          ,evalState
                                          ,MonadTrans (..)
                                          ,MonadState (..)
                                          ,evalStateT)
import Data.Maybe                         (isNothing)
import Data.String                        (IsString (..))

-- | A intermediate expression language for text templates where expressions
-- (`FillingExp`) fill their holes.
type TemplateExp = Template Text FillingExp

instance ToTemplate Text FillingExp TemplateExp where
    toTemplate :: TemplateExp -> Template Text FillingExp
    toTemplate = id

-- | Hole fillings consist of meta-variables or literals which is anything that
-- can be converted into a `Data.Text.Text`.
data FillingExp 
    = VarFilling String  -- ^ Meta-variable
    | LitFilling Text    -- ^ Literal filling

instance HoleFillingExp Text FillingExp where
    hfExpToText :: FillingExp -> Text
    hfExpToText (VarFilling v) = DT.pack v
    hfExpToText (LitFilling t) = t
    
    parseHFExp :: Maybe (Text -> Either Text FillingExp)
    parseHFExp = Just $ StrT.runParsecT (flip evalState []) fillingExpParser

instance Show FillingExp where
    show :: FillingExp -> String
    show (VarFilling x) = x
    show (LitFilling x) = show x

instance Eq FillingExp where
    -- | Equality is alpha-equivalence.
    (==) :: FillingExp -> FillingExp -> Bool
    (VarFilling _)  == (VarFilling _)  = True
    (LitFilling l1) == (LitFilling l2) = l1 == l2
    _               == _               = False

-- * JSON Syntax

-- | Type of fields of a JSON object.
type Field = (DT.Text,Either TemplateExp Value)

-- | JSON Value
data Value 
    = ObjV   [Field]    -- ^ Object
    | ArrayV [Either TemplateExp Value] -- ^ Array
    | StrV   DT.Text    -- ^ String    
    | NumV   Double     -- ^ Number
    | BoolV  Bool       -- ^ Boolean
    | NullV             -- ^ Null
    deriving (Show, Eq)

-- | Create a template for a JSON value.
value :: Value -> TemplateExp
value (ObjV   obj)   = object obj
value (ArrayV ary)   = array ary
value (StrV   s)     = chunk $ StrT.doubleQuote s
value (NumV   n)     = chunk . StrT.prettyDouble $ n
value (BoolV  True)  = chunk "true"
value (BoolV  False) = chunk "false"
value NullV          = chunk "null"

instance ToTemplate Text FillingExp Field where
    toTemplate :: Field -> Template Text FillingExp
    toTemplate = field

instance ToTemplate Text FillingExp Value where
    toTemplate :: Value -> Template Text FillingExp
    toTemplate = value

-- * Creating JSON templates

-- | Create a template from a JSON object.
object :: [Field] -- ^ List of fields of the object
       -> TemplateExp
object fields = StrT.betweenTemplate (chunk "{") (chunk "}") $ StrT.sepTemplatesBy @Text @FillingExp (chunk ",") fields

-- | Create a template of a field of an object.
field :: Field -- ^ Field of the object
      -> TemplateExp
field (label,value) = fieldLabel label +> toTemplate value

-- | Create a template of a field label of a field of an object.
fieldLabel :: DT.Text -- ^ Label of the field
           -> TemplateExp
fieldLabel = chunk . (<> ":") . StrT.doubleQuote

-- | Create a template of an array value.
array :: [Either TemplateExp Value] -- ^ List of values of the array
      -> TemplateExp
array = StrT.bracketTemplate . StrT.sepTemplatesBy @Text @FillingExp (chunk ",")

-- * Quasi-quoter for JSON templates

-- | The JSON Templates quasi-quoter.
jsonTemplate :: TH.QuasiQuoter
jsonTemplate = TH.QuasiQuoter {
     quoteExp = jsonTemplate2QExp
    ,quotePat = undefined
    ,quoteDec = undefined
    ,quoteType = undefined
}

instance StrT.ToQExp FillingExp where
    toQExp :: FillingExp -> TH.Q TH.Exp
    toQExp (VarFilling v) = TH.varE (TH.mkName v)
    toQExp (LitFilling l) = TH.litE (TH.StringL . DT.unpack $ l)
    
instance StrT.TemplateQExp Text FillingExp where

-- | Parse and convert a string into a JSON template. First parses the input
-- string into the internal language of JSON values, and then converts the
-- parsed value into a template.
jsonTemplate2QExp :: String
                  -> TH.Q TH.Exp
jsonTemplate2QExp = flip (.) (parseJSONTemplate . DT.pack) $ \case {
         Right v  -> StrT.template2QExp . value $ v
        ;Left err -> fail $ DT.unpack err
    } 

-- * JSON Templates JSONParser

-- | Parse errors
data JTParseError 
    = JTPEUnicode DT.Text
    | JTPEInvalidEscapeChar
    | JTPEDuplicateField DT.Text
    | JTPELeadingZeros
    | JTPEInvalidVarName
    deriving (Eq,Ord,Show)

instance ShowErrorComponent JTParseError where
    showErrorComponent :: JTParseError -> String
    showErrorComponent (JTPEUnicode s)        = DT.unpack s
    showErrorComponent JTPEInvalidEscapeChar  = "invalid escape character"
    showErrorComponent (JTPEDuplicateField s) = "duplicate field: "<>(DT.unpack s)
    showErrorComponent JTPELeadingZeros       = "invalid number: leading zeros are not allowed"
    showErrorComponent JTPEInvalidVarName     = "variables must begin with a lower-case letter and only consist of ASCII alpha-numeric characters"

-- | Type of tokens.
type Tok = DT.Text
-- | Type of parse errors.
type ParseError = ParseErrorBundle Tok JTParseError
-- | Type of the parsers that operate on a stream of `Tok`. The state holds onto
-- which fields have been parsed when parsing an object.
type JSONParser a = ParsecT JTParseError Text (State [DT.Text]) a

-- | Parse a string using the input parser.
parse :: JSONParser a -> DT.Text -> Either ParseError a
parse p s = evalState (runParserT p "" s) []

-- | Test a parser on some input. Useful for testing parsers in GHCi.
parseTest :: Show a => JSONParser a -> DT.Text -> IO ()
parseTest p s = do
    either 
        (putStr . errorBundlePretty) 
        print 
    $ parse p s

-- | Run the input parser against a file. This is useful for testing.
parseTestFile :: Show a => JSONParser a -> FilePath -> IO ()
parseTestFile p file = do
    f <- readFile file
    parseTest p (DT.pack f)

-- | A hole filling meta-variable.
varFexp :: String -> FillingExp
varFexp = VarFilling

-- | Convert `FillingExp` into its AST as a `Text`.
showASTFilling :: FillingExp -> Text
showASTFilling (LitFilling s) = "LitFilling "<>DT.show s
showASTFilling (VarFilling v) = "VarFilling "<>DT.show v

-- | Parses meta-variable that can fill holes.
parseVarFilling :: Text -> Either Text String
parseVarFilling s 
    = case parse varFillingParser s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right (VarFilling t) -> Right t
        _ -> error "TextTemplates.Parser: impossible branch reached in parseVarFilling."

-- | Parses a literal hole filling.
parseLitFilling :: Text -> Either Text Text
parseLitFilling s 
    = case parse litFillingParser s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right (LitFilling t) -> Right t
        _ -> error "TextTemplates.Parser: impossible branch reached in parseLitFilling."

-- | Parses a hole filling: either a meta-variable or a literal filling.
parseFillingExp :: Text -> Either Text FillingExp
parseFillingExp s 
    = case parse fillingExpParser s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right t -> Right t

-- | Parses a single character of some hole filling where @"@ and @\\@ are escaped.
charFillingParser :: JSONParser Char
charFillingParser = choice [
        satisfy (\c -> c /= '"' && c /= '\\'),
        escapeCharFillingParser
    ]

-- | Parses an escape filling character.
escapeCharFillingParser :: JSONParser Char
escapeCharFillingParser = do
    skip (string "\\")
    satisfy (`elem` ['"','\\'])

-- | Parses zero or more filling characters.
stringFillingParser :: JSONParser Text
stringFillingParser = DT.pack <$> many charFillingParser

-- | Parser for meta-variable hole fillings.
varFillingParser :: JSONParser FillingExp
varFillingParser = VarFilling <$> do
    -- Make sure we start with a lower-case ascii letter.
    c <- StrT.maybeParser . lookAhead $ takeWhile1P Nothing isAsciiLower
    if isNothing c
    then customFailure JTPEInvalidVarName
    else DT.unpack <$> takeWhile1P Nothing (\c -> isAlphaNum c && isAscii c)

-- | Parser for literal hole filling.
litFillingParser :: JSONParser FillingExp
litFillingParser = LitFilling <$> doubleQuotedParser stringFillingParser

-- | Parser for hole filling: either a meta-variable or a literal.
fillingExpParser :: JSONParser FillingExp
fillingExpParser =  litFillingParser
                <|> varFillingParser

-- | The JSON parser.
parseJSONTemplate 
    :: DT.Text -- ^ Text to parse
    -> Either DT.Text Value
parseJSONTemplate (DT.stripStart->s) 
    = case parse valueParser s of
        Left bundle -> error $ errorBundlePretty bundle
        Right s -> Right s

-- | Parser for JSON. Requires the input to end of `eof`.
jsonParser :: JSONParser Value
jsonParser = do
    space
    v <- valueParser
    eof
    pure v

-- | Parse a JSON value
valueParser :: JSONParser Value
valueParser =  do 
    v <-       (objVParser   <?> "object")
           <|> (strVParser   <?> "string")
           <|> (arrayVParser <?> "array")                             
           <|> (numVParser   <?> "number")
           <|> (boolVParser  <?> "boolean")
           <|> (nullVParser  <?> "null")
    space
    pure v

-- | Parse an object.
objectParser :: JSONParser [Field]
objectParser = do 
    -- Duplicate labels only affect the labels of the outer most object, and not
    -- nested objects. Thus, we reset the set of existing labels when we start
    -- parsing a new object.   
    lift $ put [] 
    bracesParser fieldsParser

-- | Parse a list of fields found in an object.
fieldsParser :: JSONParser [Field]
fieldsParser = sepBy fieldParser commaTok

-- | Parse a field of an object.
fieldParser :: JSONParser Field
fieldParser = do
    l <- fieldLabelParser
    existingLabels <- get
    -- Is `l` a duplicate field?
    if l `elem` existingLabels
    then customFailure $ JTPEDuplicateField l
    else do skip colonTok
            v <- valueTUParser
            -- Add `l` to the set of existing labels.
            put $ l:existingLabels
            pure $ (l,v)

-- | Parse a field label found in a field of an object.
fieldLabelParser :: JSONParser DT.Text
fieldLabelParser = doubleQuotedParser charsParser

-- | Parse an object value of a field of an object.
objVParser :: JSONParser Value
objVParser = ObjV <$> objectParser

-- | Parse a string value of a field of an object.
strVParser :: JSONParser Value
strVParser = StrV <$> doubleQuotedParser charsParser

-- | Parse a number value of a field of an object.
numVParser :: JSONParser Value
numVParser = do
    -- Try to lookahead up until any decimal point, then we can check for
    -- leading zeros.
    c <- try $ lookAhead $ takeWhileP Nothing isDigit
    dt <- try signedFloat <|> signedDecimal
    case c of 
        -- Check for leading zeros.
        ('0' DT.:< d DT.:< _) | isDigit d -> customFailure JTPELeadingZeros
        _ -> pure $ NumV dt
    where
        signedDecimal :: JSONParser Double
        signedDecimal = signed space decimal

        signedFloat :: JSONParser Double
        signedFloat = signed space float

-- | Parse a boolean value of a field of an object.
boolVParser :: JSONParser Value
boolVParser = do
    dt <- trueTok <|> falseTok
    pure . BoolV $ case dt of
                    "true" -> True
                    "false" -> False
                    _ -> error "boolVParser: impossible branch"

-- | Parse a null value of a field of an object.
nullVParser :: JSONParser Value
nullVParser =  nullTok
            *> pure NullV

-- | Parse a `Value` or `Template`.
valueTUParser :: JSONParser (Either TemplateExp Value)
valueTUParser = try 
              $  (Left  <$> templateParser) 
             <|> (Right <$> valueParser)

-- | Parse an array value of a field of an object.
arrayVParser :: JSONParser Value
arrayVParser = do 
    ary <- bracketsParser $ flip sepBy commaTok valueTUParser
    pure $ ArrayV ary

-- * JSONParser combinators

-- | Parser for JSON templates.
templateParser :: JSONParser TemplateExp
templateParser = do
    s <- singleQuotedParser charsParser
    case StrT.parseTemplate s of
        Left err -> fail . DT.unpack $ err
        Right t -> pure $ t

-- | Parse a single-quoted output of the input parser.
singleQuotedParser :: JSONParser a -> JSONParser a
singleQuotedParser = between (string "'") (tok "'")

-- | Parse a double-quoted output of the input parser.
doubleQuotedParser :: JSONParser a -> JSONParser a
doubleQuotedParser = between (string "\"") (tok "\"")

-- | Parse a braced output of the input parser.
bracesParser :: JSONParser a -> JSONParser a
bracesParser = between (tok "{") (tok "}")

-- | Parse a bracketed output of the input parser.
bracketsParser :: JSONParser a -> JSONParser a
bracketsParser = between (tok "[") (tok "]")

-- | Parse an escape character. 
-- These are one of
-- > ["\\","/",""","\'","b","n","f","r","t"]
escapeParser :: JSONParser Char
escapeParser = do
    skip backslashTok
    escapeCharParser
        <|> unicodeEscapeParser

-- | Parse an escape character without the proceeding `"\\"`.
escapeCharParser :: JSONParser Char
escapeCharParser = do
    e <- satisfy isEscapeChar
    case escapeToChar e of
        Just c -> pure c
        Nothing -> customFailure JTPEInvalidEscapeChar

-- | Predicate defining JSON escape characters.
isEscapeChar :: Char -> Bool
isEscapeChar = (`elem` ['/','\\','"','\'','b','n','f','r','t'])

-- | Properly escapes the input character based on the JSON standard.
escapeToChar :: Char -> Maybe Char
escapeToChar 'b' = Just '\b'
escapeToChar 'n' = Just '\n'
escapeToChar 'f' = Just '\f'
escapeToChar 'r' = Just '\r'
escapeToChar 't' = Just '\t'
escapeToChar '\\' = Just '\\'
escapeToChar '/' = Just '/'
escapeToChar '\'' = Just '\''
escapeToChar '"'  = Just '"'
escapeToChar _    = Nothing

-- | Parse a unicode hex string of the form @uXXXX@ into the hex string
-- @0xXXXX@.
hexCodeParser :: JSONParser String
hexCodeParser = do
    skip "u"
    d1 <- satisfy isHexDigit
    d2 <- satisfy isHexDigit
    d3 <- satisfy isHexDigit
    d4 <- satisfy isHexDigit
    pure $ "0x" <> [d1,d2,d3,d4]

-- |  Parse a unicode escape character of the form @\uXXXX@. This does handle
-- surrogate pairs. 
unicodeEscapeParser :: JSONParser Char
unicodeEscapeParser = do
    code1 <- hexCodeParser
    let i  = read code1 :: Int
    if i >= 0xD800 && i <= 0xDBFF
    then do -- Parsed high
            skip backslashTok
            code2 <- hexCodeParser    
            let j  = read code2 :: Int
            if j >= 0xDC00 && j <= 0xDFFF
            then do -- Parsed low            
                    let c = 0x10000 + (i - 0xD800) * 0x400 + (j - 0xDC00)   
                    pure . chr $ c
            else customFailure . JTPEUnicode $ "expected a low surrogate"
    else if i >= 0xDC00 && i <= 0xDFFF
         then customFailure . JTPEUnicode $ "lone low surrogate"
         else -- BMP character
              pure . chr $ i

-- | Parse a single unicode character including escapes.
charParser :: JSONParser Char
charParser = choice [
        satisfy (\c -> not (c `elem` ['\\','"','\'']) && isPrint c),
        escapeParser
    ]

-- | Parse as many unicode characters as possible including escaped characters.
charsParser :: JSONParser DT.Text
charsParser = DT.pack <$> many charParser 

-- * Tokens

-- | Parse a token (unicode character)
-- Consumes whitespace *after* the parsed token.
tok :: Tok -> JSONParser Tok
tok = symbol space

-- | Parse and throw away the symbol parsed by the input token
skip :: JSONParser Tok -> JSONParser ()
skip = skipCount 1

-- | Parse the comma token. Consumes whitespace after the parsed token.
commaTok :: JSONParser Tok
commaTok = tok ","

-- | Parse the "true" token. Consumes whitespace after the parsed token.
trueTok :: JSONParser Tok
trueTok = tok "true"

-- | Parse the "false" token. Consumes whitespace after the parsed token.
falseTok :: JSONParser Tok
falseTok = tok "false"

-- | Parse the "null" token. Consumes whitespace after the parsed token.
nullTok :: JSONParser Tok
nullTok = tok "null"

-- | Parse the colon token. Consumes whitespace after the parsed token.
colonTok :: JSONParser Tok
colonTok = tok ":"

-- | Parse the backslash token.
backslashTok :: JSONParser Tok
backslashTok = string "\\"
