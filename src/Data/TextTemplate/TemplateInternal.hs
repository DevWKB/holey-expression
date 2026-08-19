{-|
Module      : Template
Description : Framework for creating text templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

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
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.TextTemplate.TemplateInternal where

import Prelude                    hiding (null)
import Data.Text                  (Text)
import Data.Text                  qualified as DT
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
data ITemplate text where
    IChunk   :: text -> ITemplate text
    ICompose :: text -> Natural -> ITemplate text -> ITemplate text

-- | A template with pluggable holes. We do not expose the underlying
-- constructor in favor of the combinators.
data Template text filling where
    Template :: ITemplate text        -- ^ Internal template
             -> HoleProps filling     -- ^ Empty holes and hole-filling map
             -> Template text filling

instance (TextLike text, HoleFillingExp text filling) => Show (Template text filling) where
    show :: Template text filling -> String    
    show (Template (IChunk t) _) = show . toText $ t    
    show (Template (ICompose prefix i rest) (emptyHoles, filledHoles))
        = (show . toText $ prefix) 
        <> "$" <> show i <> "{"
        <> (if i `elem` emptyHoles then "" else (show . toText . (hfExpToText @text) $ filledHoles ! i))
        <> "}" 
        <> show (Template rest (emptyHoles, filledHoles))

-- * Combinators

-- | Pattern synonym for the empty template.
pattern Empty :: (Eq text, Monoid text) => Template text filling
pattern Empty <- (null -> True) where
    Empty = empty

isChunk :: Template text filling -> Maybe text
isChunk (Template (IChunk s) ([],m)) | M.null m = Just s
isChunk _ = Nothing

-- | Pattern synonym for template chunk's.
pattern Chunk :: text -> Template text filling
pattern Chunk s <- (isChunk -> Just s)
    where
        Chunk = chunk

-- | Pattern synonym for the composition of templates.
pattern Compose :: Monoid text => text -> (Natural,Maybe filling) -> Template text filling -> Template text filling
pattern Compose c h t <- (decompose -> Just (c, h, t))
    where
        Compose = compose

{-# COMPLETE Chunk, Compose #-}

-- | Explicitly create a top-level composition template.
compose :: Monoid text
        => text                   -- ^ Prefix chunk
        -> (Natural,Maybe filling)           
        -> Template text filling  -- ^ Template branch
        -> Template text filling
compose c (i, Nothing) t = chunk c +> hole i     +> t
compose c (i, Just f)  t = chunk c +> filled i f +> t

-- | Decompose a template into the top-level compose.
decompose :: Template text filling 
          -> Maybe (text, (Natural,Maybe filling), Template text filling)
decompose (Template (ICompose c i t') hlsProps) =     
     case (i,hlsProps) of
        (EmptyHole _ (uh,fh))    -> Just (c, (i,Nothing), Template t' (i `L.delete` uh,fh))
        (FilledHole _ f (uh,fh)) -> Just (c, (i,Just f),  Template t' (uh,i `delete` fh))
        (UndefHole  _ _)         -> Nothing
decompose _ = Nothing

-- | Decide if an element of a monoid is the unit.
isUnit :: (Eq m, Monoid m) 
        => m 
        -> Bool
isUnit m | m == mempty = True
         | otherwise   = False

-- | Test to see if a template is empty.
null :: (Eq text, Monoid text) => Template text filling -> Bool
null (Template (IChunk c) ([],m)) | isUnit c && M.null m = True
null _ = False

-- | Equality of `ITemplates`. Hole indices and fillings are not included in the
-- decision.
(>==>) :: Eq text 
       => ITemplate text 
       -> ITemplate text
       -> Bool
(IChunk chk1)        >==> (IChunk chk2)        = chk1 == chk2
(ICompose chk1 _ r1) >==> (ICompose chk2 _ r2) = chk1 == chk2 && r1 >==> r2
_                    >==> _                    = False

instance (Eq text, Eq filling) => Eq (Template text filling) where
    (==) :: Template text filling -> Template text filling -> Bool
    (==) = (==>)

-- | Equality of templates. Two templates are considered equivalent if and only
-- if they differ by hole labels only. The contents of filled holes are included
-- in the decision.
(==>) :: (Eq text,Eq filling)
      => Template text filling
      -> Template text filling
      -> Bool
(Template t1 (hls1,fhls1)) ==> (Template t2 (hls2,fhls2)) = t1 >==> t2 && hls1 == hls2 && fhls1 == fhls2

-- | An empty hole.
hole :: Monoid text
     => Natural -- ^ Hole index
     -> Template text filling
hole i = flip Template ([i],M.empty) $ ICompose mempty i (IChunk mempty)

-- | A hole filled with a value. 
filled :: Monoid text
       => Natural -- ^ Hole index
       -> filling -- ^ Hole filling
       -> Template text filling
filled i f 
    = flip Template ([],singleton i f) $ (ICompose mempty i (IChunk mempty))

-- | A chunk is a substring to a larger string.
chunk :: text -- ^ Substring.
      -> Template text filling
chunk = flip Template ([],M.empty) .  IChunk

-- | The empty template corresponds to the empty string.
empty :: Monoid text 
      => Template text filling
empty = chunk mempty

-- | Composition of `ITemplates`.
(>+>) :: Semigroup text
      => ITemplate text 
      -> ITemplate text 
      -> ITemplate text 
(IChunk chk1)    >+> (IChunk chk2)    = IChunk $ chk1 <> chk2
(IChunk chk)     >+> (ICompose p h r) = ICompose (chk <> p) h r
(ICompose p h r) >+> t                = ICompose p h $ r >+> t

-- | Composition of templates.
(+>) :: Semigroup text 
     => Template text filling
     -> Template text filling
     -> Template text filling
(Template t1 (ufhs1,fhs1)) +> (Template t2 (ufhs2,fhs2)) 
    = Template (t1 >+> t2) (ufhs1 `L.union` ufhs2,fhs1 `M.union` fhs2) 

instance Semigroup text => Semigroup (Template text filling) where
    (<>) :: Template text filling -> Template text filling -> Template text filling
    (<>) = (+>)

instance Monoid text => Monoid (Template text filling) where
    mempty :: Template text filling
    mempty = empty

    mconcat :: [Template text filling] -> Template text filling
    mconcat = foldr (<>) empty

instance Functor (Template text) where
    fmap :: (filling1 -> filling2) -> Template text filling1 -> Template text filling2
    fmap f (Template t (hls,fhls)) = Template t $ (hls,M.map f fhls)

instance IsString text => IsString (Template text filling) where
    fromString :: String -> Template text filling
    fromString = Chunk . fromString

class TextLike text where
    toText :: text -> Text

instance TextLike Text where
    toText :: Text -> Text
    toText = id

instance TextLike String where
    toText :: String -> Text
    toText = DT.pack

-- | Convert a templates AST into a `Text`. The `Show` instance for `Template`
-- is set to pretty print, but for debugging it is sometimes useful to see the
-- raw AST.
showAST :: (TextLike text, TextLike filling) => Template text filling -> Text
showAST (Template (IChunk x) _)                  = "IChunk "   <> (toText x)
showAST (Template (ICompose p i r) hls@(_,fhls)) = "ICompose " <> (toText p) <> " " <> (DT.show i) <> " (" <> (DT.show . (fmap toText) $ fhls !? i) <> ") (" <> (showAST (Template r hls)) <> ")"

-- | Get the list of unfilled-hole indices present in a template.
-- Time complexity: @O(0)@
unfilledHoles :: Template text filling -- ^ Template 
              -> [Natural]
unfilledHoles (Template _ (hls,_)) = hls

-- | Get the list of filled-hole indices present in a template.
-- Time complexity: @O(n)@
filledHoles :: Template text filling -- ^ Template 
            -> [Natural]
filledHoles (Template _ (_,fhls)) = keys fhls

-- | Get the filling of a hole. Returns @Nothing@ when the hole doesn't exist.
fillingInHole :: Template text filling -- ^ Template
              -> Natural               -- ^ Hole index
              -> Maybe filling
fillingInHole (Template _ (_,fhls)) h = fhls !? h

-- | Get the number of unfilled holes in a template.
-- Time complexity: @O(n)@
numberOfUnfilledHoles :: Template text filling -- ^ Template 
                      -> Int
numberOfUnfilledHoles (Template _ (hls,_)) = length hls

-- | Get the number of filled holes in a template.
-- Time complexity: @O(n)@
numberOfFilledHoles :: Template text filling -- ^ Template 
                    -> Int
numberOfFilledHoles (Template _ (_,fhls)) = M.size fhls

-- | Decide if a template is filled or not. 
-- Time complexity: \(\mathcal{O}(n)\)
isFilled :: Template text filling -> Bool
isFilled t = numberOfUnfilledHoles t == 0

-- | Convert a template with no holes, a chunk, into a text.
-- Time complexity: @O(0)@
chunkToText :: Template text filling 
            -> Maybe text
chunkToText (Template (IChunk c) ([],fhls)) | M.null fhls = Just c
chunkToText _                                             = Nothing

-- | Like `fillHole`, but doesn't update an already filled hole's value.
placeInHole :: Template text filling
            -> Natural -- ^ Hole index to plug
            -> filling -- ^ Hole filling
            -> Maybe (Template text filling)
placeInHole t@(Template it hlsProps) i c =
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ Template it $ (i `L.delete` hls,insert i c fhls)
        FilledHole _ _ _          -> Just $ t
        UndefHole  _   _          -> Nothing

-- | Fill a hole with a text. If the hole is already filled, then the filling is
-- updated with the new value. Filling a hole doesn't replace the hole, but
-- simply puts the input text inside the hole. Returns @Nothing@ if the hole
-- doesn't exist.
fillHole :: Template text filling
             -> Natural -- ^ Hole index to plug
             -> filling -- ^ Hole filling
             -> Maybe (Template text filling)
fillHole (Template t hlsProps) i c = 
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ Template t (i `L.delete` hls,insert i c fhls)
        FilledHole _ _ (hls,fhls) -> Just $ Template t (hls,insert i c fhls)
        UndefHole  _   _          -> Nothing

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHoleI :: Semigroup text
          => (filling -> text)
          -> ITemplate text
          -> [Natural]           -- ^ List of unfilled holes
          -> Natural             -- ^ Hole index to plug
          -> filling             -- ^ Text to replace hole
          -> Maybe (ITemplate text)
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
plugHole :: HoleFillingExp text filling 
         => Template text filling
         -> Natural   -- ^ Hole index to plug
         -> filling   -- ^ Text to replace hole
         -> Maybe (Template text filling)
plugHole (Template t@(ICompose _ _ _) (hls,fhls)) i c | i `elem` hls = 
        do t' <- plugHoleI hfExpToText t hls i c
           pure $ Template t' (i `L.delete` hls,fhls)
plugHole _ _ _ = Nothing

-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text).
plugAllI 
    :: Semigroup text
    => (filling -> text)
    -> [Natural]
    -> (Natural -> Maybe filling)  -- ^ Plug function.
    -> ITemplate text              -- ^ ITemplate to plug.
    -> Maybe (ITemplate text)
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
plugAll :: HoleFillingExp text filling 
        => Template text filling                     -- ^ Template to plug
        -> ([Natural] -> (Natural -> Maybe filling)) -- ^ Plug function
        -> Maybe text
plugAll (Template t (hls,fhls)) f | M.null fhls = 
    case plugAllI hfExpToText hls (f hls) t of        
        Just (IChunk c) -> Just c
        _               -> Nothing
plugAll _ _ = Nothing

-- * Template Expressions
class (Monoid text,Eq filling) => HoleFillingExp text filling  where    
    hfExpToText :: filling -> text    
    
    varHFExp :: filling -> Maybe text
    varHFExp = const Nothing

    parseHFExp :: Maybe (Text -> Either Text filling)
    parseHFExp = Nothing

class HoleFillingExp text filling => ToTemplate text filling a where
    toTemplate :: a -> Template text filling

-- | Used to add `HoleFillingExp` constraints to functions that don't take in an
-- explicit `Template`. This is useful for writing generic functions. For an
-- example, see `Data.TextTemplate.QQInternal.textTemplate2QExp`.
data Proxy filling r = Proxy {
    runProxy :: r
}

instance (ToTemplate text filling a) => ToTemplate text filling (Either (Template text filling) a) where
    toTemplate :: Either (Template text filling) a -> Template text filling
    toTemplate (Left t)  = t
    toTemplate (Right a) = toTemplate a

instance Monoid text => HoleFillingExp text () where
    hfExpToText :: () -> text
    hfExpToText () = mempty

-- | Translates a list into a template list where each template in the input
-- list is separated by the input template.
sepTemplatesBy :: (ToTemplate text filling a)
               => Template text filling   -- ^ Separator
               -> [a] -- ^ List of templates
               -> Template text filling
sepTemplatesBy _ []  = chunk mempty
sepTemplatesBy _ [v] = toTemplate v
sepTemplatesBy sep (v:vs) = toTemplate v +> sep +> sepTemplatesBy sep vs 

-- | Add a prefix and suffix templates to the given value.
betweenTemplate :: (ToTemplate text filling a) 
                => Template text filling     -- ^ Prefix template
                -> Template text filling     -- ^ Suffice template
                -> a              -- ^ Value to be converted into a template
                -> Template text filling
betweenTemplate b a (toTemplate->t) = b +> t +> a
