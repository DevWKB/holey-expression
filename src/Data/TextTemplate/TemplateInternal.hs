{-|
Module      : Template
Description : Framework for creating text templates
Copyright   : (c) Harley Eades, 2026
              (c) WꓘB, 2026
Maintainer  : harley.eades@pm.me

Framework for creating text templates. These are text with holes that 
can be filled and plugged. No parsing of the actual text is done, but the 
text is broken up into `chunk`'s in between the `hole`'s.

Notes for writing docs:
    - Hole filling parser must escape curly braces and forward slash.
-}
{-# LANGUAGE PatternSynonyms              #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE BangPatterns                 #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE PatternSynonyms              #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.TextTemplate.TemplateInternal where

import Prelude                    hiding (null)
import Data.Text                  (Text)
import Data.Text                  qualified as DT
import Data.Char                  (isAsciiLower
                                  ,isAlphaNum
                                  ,isAscii)
import Data.Maybe                 (isNothing)
import Data.String                (IsString (..))
import Data.List                  qualified as L
import Data.NatMap                (NatMap
                                  ,Natural
                                  ,(!?)
                                  ,keys
                                  ,insert
                                  ,(!)
                                  ,delete
                                  ,singleton)
import Data.NatMap                qualified as M
import Text.Megaparsec            (Parsec
                                  ,ShowErrorComponent (..)
                                  ,MonadParsec (try, takeWhile1P)
                                  ,between
                                  ,skipCount
                                  ,many
                                  ,some
                                  ,satisfy
                                  ,choice
                                  ,atEnd
                                  ,parse
                                  ,errorBundlePretty
                                  ,(<|>)
                                  ,lookAhead
                                  ,customFailure, ParsecT, ParseErrorBundle, runParserT)                                    
import Text.Megaparsec.Char       (char
                                  ,digitChar
                                  ,space
                                  ,string)
import Text.Megaparsec.Char.Lexer (symbol)
import Data.Void (Void)

type Hole f      = (Natural,HoleProps f)
type HoleProps f = ([Natural],NatMap f)

pattern EmptyHole :: Natural -> HoleProps f -> Hole f
pattern EmptyHole i hlsProps <- (decomposeEmptyHole -> Just (i,hlsProps))

pattern FilledHole :: Natural -> f -> HoleProps f -> Hole f
pattern FilledHole i f hlsProps <- (decomposeFilledHole -> Just (i,Just f,hlsProps))

pattern UndefHole :: Natural -> HoleProps f -> Hole f
pattern UndefHole i hlsProps <- (decomposeUndefHole -> Just (i,hlsProps))

decomposeEmptyHole :: (Natural, HoleProps f) -> Maybe (Natural, HoleProps f)
decomposeEmptyHole h@(i,hlsProps) | emptyHole i hlsProps = Just h
                                  | otherwise = Nothing

decomposeFilledHole :: Hole f -> Maybe (Natural, Maybe f, HoleProps f)
decomposeFilledHole (i,hlsProps@(_,fhls)) | filledHole i hlsProps = Just (i,fhls !? i,hlsProps)
                                          | otherwise             = Nothing

decomposeUndefHole :: Hole f -> Maybe (Hole f)
decomposeUndefHole h | isNothing (decomposeEmptyHole h) && isNothing (decomposeFilledHole h) = Just h
                     | otherwise = Nothing

{-# COMPLETE EmptyHole, FilledHole, UndefHole #-}

-- | Tests to see if a hole index exist in the given hole properties.
isFreshHoleIndex :: Natural     -- ^ Hole index
                 -> HoleProps f -- ^ Hole properties
                 -> Bool
isFreshHoleIndex h holeProps = not $ filledHole h holeProps || emptyHole h holeProps

emptyHole :: Natural -> HoleProps f -> Bool
emptyHole i (hls,fhls) = i `elem` hls && not (i `elem` keys fhls)

filledHole :: Natural -> HoleProps f -> Bool
filledHole i (hls,fhls) = not (i `elem` hls) && i `elem` keys fhls

emptyHoleProps :: HoleProps f
emptyHoleProps = ([], M.empty)

-- | Adds a hole index and potential filling to the given hole properties. If
-- the given filling is @Nothing@ then the hole is assumed to be added as an
-- unfilled hole, otherwise it's added as a filled hole. The given index cannot
-- already exist in the hole properties.
updateFreshHolePropsWith 
    :: HoleProps Text 
    -> (Natural,Maybe Text) 
    -> HoleProps Text
updateFreshHolePropsWith holeProps@(hls, fhls) (h, Nothing)  | h `isFreshHoleIndex` holeProps = (h:hls,fhls)
updateFreshHolePropsWith holeProps@(hls, fhls) (h, (Just f)) | h `isFreshHoleIndex` holeProps = (hls,insert h f fhls)
updateFreshHolePropsWith holeProps             (_,_)                                          = holeProps

-- | Internal templates are the underlying structure of `Template`.
data ITemplate where
    IChunk   :: Text -> ITemplate 
    ICompose :: Text -> Natural -> ITemplate -> ITemplate

-- | A template with pluggable holes. We do not expose the underlying
-- constructor in favor of the combinators.
data Template f where
    Template :: ITemplate         -- ^ Internal template
             -> HoleProps f       -- ^ Empty holes and hole-filling map
             -> Template f

instance HoleFillingExp f => Show (Template f) where
    show :: Template f -> String    
    show (Template (IChunk t) _) = DT.unpack t    
    show (Template (ICompose prefix i rest) (emptyHoles, filledHoles))
        = DT.unpack prefix <> "$" <> show i <> "{"
                           <> (if i `elem` emptyHoles then "" else (DT.unpack . hfExpToText $ filledHoles ! i))
                           <> "}" <> show (Template rest (emptyHoles, filledHoles))

-- * Combinators

-- | Pattern synonym for the empty template.
pattern Empty :: Template f
pattern Empty <- (null -> True) where
    Empty = empty

isChunk :: Template f -> Maybe Text
isChunk (Template (IChunk s) ([],m)) | M.null m = Just s
isChunk _ = Nothing

-- | Pattern synonym for template chunk's.
pattern Chunk :: Text -> Template f
pattern Chunk s <- (isChunk -> Just s)
    where
        Chunk = chunk

-- | Pattern synonym for the composition of templates.
pattern Compose :: Text -> (Natural,Maybe f) -> Template f -> Template f
pattern Compose c h t <- (decompose -> Just (c, h, t))
    where
        Compose = compose

{-# COMPLETE Chunk, Compose #-}

-- | Explicitly create a top-level composition template.
compose :: Text             -- ^ Prefix chunk
        -> (Natural,Maybe f)           
        -> Template f       -- ^ Template branch
        -> Template f
compose c (i, Nothing) t = chunk c +> hole i     +> t
compose c (i, Just f)  t = chunk c +> filled i f +> t

-- | Decompose a template into the top-level compose.
decompose :: Template f -> Maybe (Text, (Natural,Maybe f), Template f)
decompose (Template (ICompose c i t') hlsProps) =     
     case (i,hlsProps) of
        (EmptyHole _ (uh,fh))    -> Just (c, (i,Nothing), Template t' (i `L.delete` uh,fh))
        (FilledHole _ f (uh,fh)) -> Just (c, (i,Just f),  Template t' (uh,i `delete` fh))
        (UndefHole  _ _)         -> Nothing
decompose _ = Nothing

-- | Test to see if a template is empty.
null :: Template f -> Bool
null (Template (IChunk "") ([],m)) | M.null m = True
null _ = False

-- | Equality of `ITemplates`. Hole indices are not included in the decision.
(>==>) :: ITemplate
       -> ITemplate
       -> Bool
(IChunk chk1)          >==> (IChunk chk2)          = chk1 == chk2
(ICompose   chk1 _ r1) >==> (ICompose   chk2 _ r2) = chk1 == chk2 && r1 >==> r2
_                      >==> _                      = False

instance Eq f => Eq (Template f) where
    (==) :: Template f -> Template f -> Bool
    (==) = (==>)

-- | Equality of templates. Two templates are considered equivalent if and only
-- if they differ by hole labels only. The contents of filled holes are included
-- in the decision.
(==>) :: Eq f
      => Template f
      -> Template f
      -> Bool
(Template t1 (hls1,fhls1)) ==> (Template t2 (hls2,fhls2)) = t1 >==> t2 && hls1 == hls2 && fhls1 == fhls2

-- | An empty hole.
hole :: Natural -- ^ Hole index
     -> Template f
hole (i) = flip Template ([i],M.empty) $ ICompose "" i (IChunk "")

-- | A hole filled with a value. 
filled :: Natural -- ^ Hole index
       -> f       -- ^ Hole filling
       -> Template f
filled i f 
    = flip Template ([],singleton i f) $ (ICompose "" i (IChunk ""))

-- | A chunk is a substring to a larger string.
chunk :: Text -- ^ Substring.
      -> Template f
chunk = flip Template ([],M.empty) .  IChunk

-- | The empty template corresponds to the empty string.
empty :: Template f
empty = chunk ""

-- | Composition of `ITemplates`.
(>+>) :: ITemplate 
      -> ITemplate 
      -> ITemplate 
(IChunk chk1)    >+> (IChunk chk2)    = IChunk $ chk1 <> chk2
(IChunk chk)     >+> (ICompose p h r) = ICompose (chk <> p) h r
(ICompose p h r) >+> t                = ICompose p h $ r >+> t

-- | Composition of templates.
(+>) :: Template f
     -> Template f
     -> Template f
(Template t1 (ufhs1,fhs1)) +> (Template t2 (ufhs2,fhs2)) 
    = Template (t1 >+> t2) (ufhs1 `L.union` ufhs2,fhs1 `M.union` fhs2) 

instance Semigroup (Template f) where
    (<>) :: Template f -> Template f -> Template f
    (<>) = (+>)

instance Monoid (Template f) where
    mempty :: Template f
    mempty = empty

    mconcat :: [Template f] -> Template f
    mconcat = foldr (<>) empty

instance Functor Template where
    fmap :: (a -> b) -> Template a -> Template b
    fmap f (Template t (hls,fhls)) = Template t $ (hls,M.map f fhls)

instance IsString (Template f) where
    fromString :: String -> Template f
    fromString = Chunk . DT.pack

-- | Convert a templates AST into a `Text`. The `Show` instance for `Template`
-- is set to pretty print, but for debugging it is sometimes useful to see the
-- raw AST.
showAST :: Show f => Template f -> Text
showAST (Template (IChunk x) _)                  = "IChunk "   <> (DT.show x)
showAST (Template (ICompose p i r) hls@(_,fhls)) = "ICompose " <> (DT.show p) <> " " <> (DT.show i) <> " (" <> (DT.show $ fhls !? i) <> ") (" <> (showAST (Template r hls)) <> ")"

-- | Get the list of unfilled-hole indices present in a template.
-- Time complexity: @O(0)@
unfilledHoles :: Template f -- ^ Template 
              -> [Natural]
unfilledHoles (Template _ (hls,_)) = hls

-- | Get the list of filled-hole indices present in a template.
-- Time complexity: @O(n)@
filledHoles :: Template f -- ^ Template 
            -> [Natural]
filledHoles (Template _ (_,fhls)) = keys fhls

-- | Get the filling of a hole. Returns @Nothing@ when the hole doesn't exist.
fillingInHole :: Template f -- ^ Template
              -> Natural    -- ^ Hole index
              -> Maybe f
fillingInHole (Template _ (_,fhls)) h = fhls !? h

-- | Get the number of unfilled holes in a template.
-- Time complexity: @O(n)@
numberOfUnfilledHoles :: Template f -- ^ Template 
                      -> Int
numberOfUnfilledHoles (Template _ (hls,_)) = length hls

-- | Get the number of filled holes in a template.
-- Time complexity: @O(n)@
numberOfFilledHoles :: Template f -- ^ Template 
                    -> Int
numberOfFilledHoles (Template _ (_,fhls)) = M.size fhls

-- | Decide if a template is filled or not. 
-- Time complexity: \(\mathcal{O}(n)\)
isFilled :: Template f -> Bool
isFilled t = numberOfUnfilledHoles t == 0

-- | Convert a template with no holes, a chunk, into a text.
-- Time complexity: @O(0)@
chunkToText :: Template f     
            -> Maybe Text
chunkToText (Template (IChunk c) ([],fhls)) | M.null fhls = Just c
chunkToText _                                             = Nothing

-- | Like `fillHole`, but doesn't update an already filled hole's value.
placeInHole :: Template f
            -> Natural -- ^ Hole index to plug
            -> f       -- ^ Holeilling
            -> Maybe (Template f)
placeInHole t@(Template it hlsProps) i c =
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ Template it $ (i `L.delete` hls,insert i c fhls)
        FilledHole _ _ _          -> Just $ t
        UndefHole  _   _          -> Nothing

-- | Fill a hole with a text. If the hole is already filled, then the filling is
-- updated with the new value. Filling a hole doesn't replace the hole, but
-- simply puts the input text inside the hole. Returns @Nothing@ if the hole
-- doesn't exist.
fillHole :: Template f
             -> Natural -- ^ Hole index to plug
             -> f       -- ^ Holeilling
             -> Maybe (Template f)
fillHole (Template t hlsProps) i c = 
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ Template t (i `L.delete` hls,insert i c fhls)
        FilledHole _ _ (hls,fhls) -> Just $ Template t (hls,insert i c fhls)
        UndefHole  _   _          -> Nothing

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHoleI :: (f -> Text)
          -> ITemplate
          -> [Natural]           -- ^ List of unfilled holes
          -> Natural             -- ^ Hole index to plug
          -> f                   -- ^ Text to replace hole
          -> Maybe ITemplate
plugHoleI toText (ICompose p h (IChunk s)) hls i c 
    | i == h && h `elem` hls = Just $ IChunk $ p <> toText c <> s
plugHoleI toText (ICompose p h r@(ICompose p' h' s)) hls i c 
    | i == h && h `elem` hls = Just $ ICompose (p <> toText c <> p') h' s
    | otherwise = do r' <- plugHoleI toText r hls i c
                     Just $ ICompose p h r'
plugHoleI _ _ _ _ _ = Nothing       

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHole :: HoleFillingExp hfExp => Template hfExp
         -> Natural -- ^ Hole index to plug
         -> hfExp   -- ^ Text to replace hole
         -> Maybe (Template hfExp)
plugHole (Template t@(ICompose _ _ _) (hls,fhls)) i c | i `elem` hls = 
        do t' <- plugHoleI hfExpToText t hls i c
           pure $ Template t' (i `L.delete` hls,fhls)
plugHole _ _ _ = Nothing

-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text).
plugAllI 
    :: (f -> Text)
    -> [Natural]
    -> (Natural -> Maybe f)  -- ^ Plug function.
    -> ITemplate             -- ^ ITemplate to plug.
    -> Maybe ITemplate
plugAllI toText hls f (ICompose chk i r) | i `elem` hls = do
    chk' <- f i
    IChunk chk'' <- plugAllI toText hls f r
    return . IChunk $ chk <> toText chk' <> chk''
plugAllI _ _ _ (ICompose _ _ _) = Nothing
plugAllI _ _ _ t@(IChunk _) = return t

-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text) is
-- returned.
plugAll :: HoleFillingExp hfExp 
        => Template hfExp  -- ^ Template to plug
        -> ([Natural] -> (Natural -> Maybe hfExp)) -- ^ Plug function
        -> Maybe Text
plugAll (Template t (hls,fhls)) f | M.null fhls = 
    case plugAllI hfExpToText hls (f hls) t of        
        Just (IChunk c) -> Just c
        _              -> Nothing
plugAll _ _ = Nothing

-- * Template Expressions
class Eq hfExp => HoleFillingExp hfExp where    
    hfExpToText :: hfExp -> Text    
    
    varHFExp :: hfExp -> Maybe String
    varHFExp = const Nothing

    parseHFExp :: Maybe (Text -> Either Text hfExp)
    parseHFExp = Nothing

class HoleFillingExp hfExp => ToTemplate hfExp a where
    toTemplate :: a -> Template hfExp

-- | Used to add `HoleFillingExp` constraints to functions that don't take in an
-- explicit `Template`. This is useful for writing generic functions. For an
-- example, see `Data.TextTemplate.QQInternal.textTemplate2QExp`.
data Proxy hfExp r = Proxy {
    runProxy :: r
}

instance (ToTemplate hfExp a) => ToTemplate hfExp (Either (Template hfExp) a) where
    toTemplate :: Either (Template hfExp) a -> Template hfExp
    toTemplate (Left t)  = t
    toTemplate (Right a) = toTemplate a

instance HoleFillingExp () where
    hfExpToText :: () -> Text
    hfExpToText () = ""

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
        
instance HoleFillingExp Text where
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

instance HoleFillingExp Int where
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

-- | Translates a list into a template list where each template in the input
-- list is separated by the input template.
sepTemplatesBy :: (ToTemplate hfExp a)
               => Template hfExp   -- ^ Separator
               -> [a] -- ^ List of templates
               -> Template hfExp
sepTemplatesBy _ []  = chunk ""
sepTemplatesBy _ [v] = toTemplate v
sepTemplatesBy sep (v:vs) = toTemplate v +> sep +> sepTemplatesBy sep vs 

-- | Add a prefix and suffix templates to the given value.
betweenTemplate :: (ToTemplate hfExp a) 
                => Template hfExp     -- ^ Prefix template
                -> Template hfExp     -- ^ Suffice template
                -> a              -- ^ Value to be converted into a template
                -> Template hfExp
betweenTemplate b a (toTemplate->t) = b +> t +> a

-- | Add brackets `[]` around the input template.
bracketTemplate :: (ToTemplate hfExp a) => a -> Template hfExp
bracketTemplate = betweenTemplate (chunk "[") (chunk "]")

-- | Add braces `{}` around the input template.
braceTemplate :: (ToTemplate hfExp a) => a -> Template hfExp
braceTemplate = betweenTemplate (chunk "{") (chunk "}")

-- | Parse a template.
parseTemplate :: HoleFillingExp hfExp => Text -> Either Text (Template hfExp)
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
holeFillingParser :: HoleFillingExp hfExp => Parser (Maybe hfExp)
holeFillingParser = maybe (pure Nothing) p parseHFExp
    where
        p :: (Text -> Either Text hfExp) -> Parser (Maybe hfExp)
        p expParser = do
            f <- between (char '{') (char '}') $ many $ templateCharParser True
            if L.null f
            then pure Nothing 
            else do let e = expParser . DT.pack $ f
                    case e of
                        Left err -> customFailure $ HFExpParseError err
                        Right f' -> pure . Just $ f'

-- | Parse a `Hole`. That is, a pair of a hole index and a filling.
holeParser :: HoleFillingExp hfExp => Parser (Natural, Maybe hfExp)
holeParser = do
    skip (string "$")
    i <- holeIndexParser
    f <- holeFillingParser
    pure $ (i, f)

-- | Parse a `Chunk`.
chunkParser :: Parser Text
chunkParser = DT.pack <$> many (templateCharParser False)

-- | Parse a template either as a `Chunk` or a `Compose`.
templateParser :: HoleFillingExp hfExp => Parser (Template hfExp)
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
doubleQuotedParser = between (string "\"") (tok "\"")

-- * Tokens

-- | Parse a token (unicode character)
-- Consumes whitespace *after* the parsed token.
tok :: Ord e => Tok -> Parsec e Tok Tok
tok = symbol space

-- | Parse and throw away the symbol parsed by the input token
skip :: Parsec e Tok Tok -> Parsec e Tok ()
skip = skipCount 1
