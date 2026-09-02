{-|
Module      : HExpInternal
Description : Internal framework for creating holey expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternSynonyms              #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE BangPatterns                 #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE PatternSynonyms              #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.HoleyExp.HExpInternal where
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

-- | Holes are either empty or filled; thus, a hole's properties consists of a
-- pair of a list of natural numbers designating the set of empty holes and a
-- natural-number map (t'NatMap') that assigns fillings of type @f@ to hole
-- indices.
type HoleProps f = ([Natural],NatMap f)

-- | A hole that can be filled with a filling of type @f@ is a natural number
-- and a set of hole properties (t`HoleProps`).
type Hole f      = (Natural,HoleProps f)

-- ** Hole Patterns

-- | Pattern synonym for empty holes. 
pattern EmptyHole :: Natural -> HoleProps f -> Hole f
pattern EmptyHole i hlsProps <- (decomposeEmptyHole -> Just (i,hlsProps))

-- | Pattern synonym for filled holes.
pattern FilledHole :: Natural -> f -> HoleProps f -> Hole f
pattern FilledHole i f hlsProps <- (decomposeFilledHole -> Just (i,Just f,hlsProps))

-- | Pattern synonym for undefined holes. These are holes which are not currently used
-- in the expression; and thus, are neither free nor empty.
pattern UndefHole :: Natural -> HoleProps f -> Hole f
pattern UndefHole i hlsProps <- (decomposeUndefHole -> Just (i,hlsProps))

-- | Determines if the input hole index @i@ is the index of an empty hole.
decomposeEmptyHole :: (Natural, HoleProps f) -> Maybe (Natural, HoleProps f)
decomposeEmptyHole h@(i,hlsProps) | emptyHole i hlsProps = Just h
                                  | otherwise = Nothing

-- | Determines if the input hole index @i@ is the index of a filled hole.
decomposeFilledHole :: Hole f -> Maybe (Natural, Maybe f, HoleProps f)
decomposeFilledHole (i,hlsProps@(_,fhls)) | filledHole i hlsProps = Just (i,fhls !? i,hlsProps)
                                          | otherwise             = Nothing

-- | Determines if the input hole index @i@ is neither an empty hole or a filled
-- hole; thus, is undefined in the expression.
decomposeUndefHole :: Hole f -> Maybe (Hole f)
decomposeUndefHole h | isNothing (decomposeEmptyHole h) && isNothing (decomposeFilledHole h) = Just h
                     | otherwise = Nothing

{-# COMPLETE EmptyHole, FilledHole, UndefHole #-}

-- | Tests to see if a hole index exist in the given hole properties.
isFreshHoleIndex :: Natural     -- ^ Hole index
                 -> HoleProps f -- ^ Hole properties
                 -> Bool
isFreshHoleIndex h holeProps = not $ filledHole h holeProps || emptyHole h holeProps

-- | Decides if the given hole index is empty with respect to the given hole
-- properties. This returns `True` when the given index is in the set of
-- empty holes, but is not defined in the map of filled holes.
emptyHole :: Natural -> HoleProps f -> Bool
emptyHole i (hls,fhls) = i `elem` hls && not (i `elem` keys fhls)

-- | Decides if the given hole index is filled with respect to the given hole
-- properties. This returns `True` when the given index is not in the set of
-- empty holes, but is defined in the map of filled holes.
filledHole :: Natural -> HoleProps f -> Bool
filledHole i (hls,fhls) = not (i `elem` hls) && i `elem` keys fhls

-- | The hole properties with no defined holes.
emptyHoleProps :: HoleProps f
emptyHoleProps = ([], M.empty)

-- | Adds a hole index and potential filling to the given hole properties. If
-- the given filling is @Nothing@ then the hole is assumed to be added as an
-- unfilled hole, otherwise it's added as a filled hole. The given index cannot
-- already exist in the hole properties.
updateFreshHolePropsWith 
    :: HoleProps text 
    -> (Natural,Maybe text) 
    -> HoleProps text
updateFreshHolePropsWith holeProps@(hls, fhls) (h, Nothing)  | h `isFreshHoleIndex` holeProps = (h:hls,fhls)
updateFreshHolePropsWith holeProps@(hls, fhls) (h, (Just f)) | h `isFreshHoleIndex` holeProps = (hls,insert h f fhls)
updateFreshHolePropsWith holeProps             (_,_)                                          = holeProps

-- | The underlying structure of t'HExp'.
data IHExp text where
    IChunk   :: text -> IHExp text
    ICompose :: text -> Natural -> IHExp text -> IHExp text

-- | An expression with pluggable holes. We do not expose the underlying
-- constructors in favor of the combinators.
data HExp text filling where
    HExp :: IHExp text        -- ^ Internal expression
         -> HoleProps filling -- ^ Empty holes and hole-filling map
         -> HExp text filling

instance (TextLike text, HoleFilling text filling) => Show (HExp text filling) where
    show :: HExp text filling -> String    
    show (HExp (IChunk t) _) = DT.unpack . toText $ t    
    show (HExp (ICompose prefix i rest) (emptyHoles, filledHoles))
        = (DT.unpack . toText $ prefix) 
        <> "$" <> show i <> "{"
        <> (if i `elem` emptyHoles then "" else (DT.unpack . toText . fToT $ filledHoles ! i))
        <> "}" 
        <> show (HExp rest (emptyHoles, filledHoles))
        where
            fToT :: filling -> text
            fToT = fillingToText

-- * Combinators

-- | Pattern synonym for the empty expression.
pattern Empty :: (Eq text, Monoid text) => HExp text filling
pattern Empty <- (null -> True) where
    Empty = emptyExp

-- | Decides if an expression corresponds to a chunk or not. 
isChunk :: HExp text filling -> Maybe text
isChunk (HExp (IChunk s) ([],m)) | M.null m = Just s
isChunk _ = Nothing

-- | Pattern synonym for expression chunk's.
pattern Chunk :: text -> HExp text filling
pattern Chunk s <- (isChunk -> Just s)
    where
        Chunk = chunk

-- | Pattern synonym for the composition of holey expressions.
pattern Compose :: Monoid text => text -> (Natural,Maybe filling) -> HExp text filling -> HExp text filling
pattern Compose c h t <- (decompose -> Just (c, h, t))
    where
        Compose = compose

{-# COMPLETE Chunk, Compose #-}

-- | Explicitly create a top-level composition expression.
compose :: Monoid text
        => text                   -- ^ Prefix chunk
        -> (Natural,Maybe filling)           
        -> HExp text filling  -- ^ HExp branch
        -> HExp text filling
compose c (i, Nothing) t = chunk c +> empty i     +> t
compose c (i, Just f)  t = chunk c +> filled i f +> t

-- | Decompose an expression into the top-level compose.
decompose :: HExp text filling 
          -> Maybe (text, (Natural,Maybe filling), HExp text filling)
decompose (HExp (ICompose c i t') hlsProps) =     
     case (i,hlsProps) of
        (EmptyHole _ (uh,fh))    -> Just (c, (i,Nothing), HExp t' (i `L.delete` uh,fh))
        (FilledHole _ f (uh,fh)) -> Just (c, (i,Just f),  HExp t' (uh,i `delete` fh))
        (UndefHole  _ _)         -> Nothing
decompose _ = Nothing

-- | Decide if an element of a monoid is the unit.
isUnit :: (Eq m, Monoid m) 
        => m 
        -> Bool
isUnit m | m == mempty = True
         | otherwise   = False

-- | Test to see if an expression is empty.
null :: (Eq text, Monoid text) => HExp text filling -> Bool
null (HExp (IChunk c) ([],m)) | isUnit c && M.null m = True
null _ = False

-- | Equality of t`IHExp`. Holes are ignored.
(>==>) :: Eq text 
       => IHExp text 
       -> IHExp text
       -> Bool
(IChunk chk1)        >==> (IChunk chk2)        = chk1 == chk2
(ICompose chk1 _ r1) >==> (ICompose chk2 _ r2) = chk1 == chk2 && r1 >==> r2
_                    >==> _                    = False

instance (Eq text, Eq filling) => Eq (HExp text filling) where
    (==) :: HExp text filling -> HExp text filling -> Bool
    (==) = (==>)

-- | Equality of holey expressions. Two holey expressions are considered equivalent if and only
-- if they differ by hole labels only. The contents of filled holes are included
-- in the decision.
(==>) :: (Eq text,Eq filling)
      => HExp text filling
      -> HExp text filling
      -> Bool
(HExp t1 (hls1,fhls1)) ==> (HExp t2 (hls2,fhls2)) = t1 >==> t2 && hls1 == hls2 && fhls1 == fhls2

-- | An empty hole.
empty :: Monoid text
     => Natural -- ^ Hole index
     -> HExp text filling
empty i = flip HExp ([i],M.empty) $ ICompose mempty i (IChunk mempty)

-- | A hole with a filling. 
filled :: Monoid text
       => Natural -- ^ Hole index
       -> filling -- ^ Hole filling
       -> HExp text filling
filled i f 
    = flip HExp ([],singleton i f) $ (ICompose mempty i (IChunk mempty))

-- | A chunk is a constant; it's helpful to think of these as a piece of
-- subtext. 
chunk :: text -- ^ Constant
      -> HExp text filling
chunk = flip HExp ([],M.empty) .  IChunk

-- | The empty expression.
emptyExp :: Monoid text 
         => HExp text filling
emptyExp = chunk mempty

-- | Composition of `IHExp`.
(>+>) :: Semigroup text
      => IHExp text 
      -> IHExp text 
      -> IHExp text 
(IChunk chk1)    >+> (IChunk chk2)    = IChunk $ chk1 <> chk2
(IChunk chk)     >+> (ICompose p h r) = ICompose (chk <> p) h r
(ICompose p h r) >+> t                = ICompose p h $ r >+> t

-- | Composition of holey expressions.
(+>) :: Semigroup text 
     => HExp text filling
     -> HExp text filling
     -> HExp text filling
(HExp t1 (ufhs1,fhs1)) +> (HExp t2 (ufhs2,fhs2)) 
    = HExp (t1 >+> t2) (ufhs1 `L.union` ufhs2,fhs1 `M.union` fhs2) 

instance Semigroup text => Semigroup (HExp text filling) where
    (<>) :: HExp text filling -> HExp text filling -> HExp text filling
    (<>) = (+>)

instance Monoid text => Monoid (HExp text filling) where
    mempty :: HExp text filling
    mempty = emptyExp

    mconcat :: [HExp text filling] -> HExp text filling
    mconcat = foldr (<>) emptyExp

instance Functor (HExp text) where
    fmap :: (filling1 -> filling2) -> HExp text filling1 -> HExp text filling2
    fmap f (HExp t (hls,fhls)) = HExp t $ (hls,M.map f fhls)

instance IsString text => IsString (HExp text filling) where
    fromString :: String -> HExp text filling
    fromString = Chunk . fromString

-- | A type is "text like" if it can be converted into t`Text`.
class TextLike text where
    toText :: text -> Text

instance TextLike Text where
    toText :: Text -> Text
    toText = id

instance TextLike String where
    toText :: String -> Text
    toText = DT.pack

instance TextLike Double where
    toText :: Double -> Text
    toText = DT.show

instance TextLike Int where
    toText :: Int -> Text
    toText = DT.show

instance TextLike Integer where
    toText :: Integer -> Text
    toText = DT.show

-- | Convert a holey expression's AST into a `Text`. The `Show` instance for
-- t`HExp` is set to pretty print, but for debugging it is sometimes useful to
-- see the raw AST.
showAST :: (TextLike text, TextLike filling) => HExp text filling -> Text
showAST (HExp (IChunk x) _)                  = "IChunk "   <> (toText x)
showAST (HExp (ICompose p i r) hls@(_,fhls)) = "ICompose " <> (toText p) <> " " <> (DT.show i) <> " (" <> (DT.show . (fmap toText) $ fhls !? i) <> ") (" <> (showAST (HExp r hls)) <> ")"

-- | Get the list of unfilled-hole indices present in an expression.
-- Time complexity: \( \mathcal{O}(0) \)
unfilledHoles :: HExp text filling -- ^ HExp 
              -> [Natural]
unfilledHoles (HExp _ (hls,_)) = hls

-- | Get the list of filled-hole indices present in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
filledHoles :: HExp text filling -- ^ HExp 
            -> [Natural]
filledHoles (HExp _ (_,fhls)) = keys fhls

-- | Get the filling of a hole. Returns @Nothing@ when the hole doesn't exist.
fillingInHole :: HExp text filling -- ^ HExp
              -> Natural           -- ^ Hole index
              -> Maybe filling
fillingInHole (HExp _ (_,fhls)) h = fhls !? h

-- | Get the number of unfilled holes in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
numberOfUnfilledHoles :: HExp text filling -- ^ HExp 
                      -> Int
numberOfUnfilledHoles (HExp _ (hls,_)) = length hls

-- | Get the number of filled holes in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
numberOfFilledHoles :: HExp text filling -- ^ HExp 
                    -> Int
numberOfFilledHoles (HExp _ (_,fhls)) = M.size fhls

-- | Decide if an expression is filled or not. 
-- Time complexity: \(\mathcal{O}(n)\)
isFilled :: HExp text filling -> Bool
isFilled t = numberOfUnfilledHoles t == 0

-- | Convert an expression with no holes, a chunk, into a text.
-- Time complexity: \( \mathcal{O}(0) \)
chunkToText :: HExp text filling 
            -> Maybe text
chunkToText (HExp (IChunk c) ([],fhls)) | M.null fhls = Just c
chunkToText _                                         = Nothing

-- | Like `update`, but doesn't update an already filled hole's value.
place :: HExp text filling
            -> Natural -- ^ Hole index to plug
            -> filling -- ^ Hole filling
            -> Maybe (HExp text filling)
place t@(HExp it hlsProps) i c =
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ HExp it $ (i `L.delete` hls,insert i c fhls)
        FilledHole _ _ _          -> Just $ t
        UndefHole  _   _          -> Nothing

-- | Update a hole adding or removing a filling. If the hole is already filled,
-- then the filling is updated with the new value. Filling a hole doesn't
-- replace the hole, but simply puts the input @filling@ inside the hole.
-- Returns @Nothing@ if the hole doesn't exist. If the input filling is
-- `Nothing`, then the hole is emptied. The complexity of this operation
-- is \(\mathcal{O}(\max(n_0,\min(n_1,W)))\), where \(n_0\) is the number of
-- empty holes, and \(n_1\) is the number of filled holes with a max of \(W\)
-- the number of bits in an `Int` (32 or 64).
update :: HExp text filling
             -> Natural       -- ^ Hole index to fill
             -> Maybe filling -- ^ Hole filling
             -> Maybe (HExp text filling)
update (HExp t hlsProps) i Nothing = 
    case (i,hlsProps) of
        EmptyHole  _   _          -> Just $ HExp t hlsProps
        FilledHole _ _ (hls,fhls) -> Just $ HExp t (i `L.insert` hls,delete i fhls)
        UndefHole  _   _          -> Nothing

update (HExp t hlsProps) i (Just c) = 
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ HExp t (i `L.delete` hls,insert i c fhls)
        FilledHole _ _ (hls,fhls) -> Just $ HExp t (hls,insert i c fhls)
        UndefHole  _   _          -> Nothing

-- | Plug an unfilled hole in an expression with some filling. Returns @Nothing@ when
-- the hole index doesn't exist in the expression or is filled, otherwise returns
-- an expression with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `update`.
plugHoleI :: Semigroup text
          => (filling -> text)
          -> IHExp text
          -> [Natural]           -- ^ List of unfilled holes
          -> Natural             -- ^ Hole index to plug
          -> filling             -- ^ Text to replace hole
          -> Maybe (IHExp text)
plugHoleI toText (ICompose p h (IChunk s)) hls i c 
    | i == h && h `elem` hls = Just $ IChunk $ p <> toText c <> s
plugHoleI toText (ICompose p h r@(ICompose p' h' s)) hls i c 
    | i == h && h `elem` hls = Just $ ICompose (p <> toText c <> p') h' s
    | otherwise = do r' <- plugHoleI toText r hls i c
                     Just $ ICompose p h r'
plugHoleI _ _ _ _ _ = Nothing       

-- | Plug an unfilled hole in an expression with some filling. Returns @Nothing@ when
-- the hole index doesn't exist in the expression or is filled, otherwise returns
-- an expression with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `update`.
plug :: HoleFilling text filling 
         => HExp text filling
         -> Natural   -- ^ Hole index to plug
         -> filling   -- ^ Text to replace hole
         -> Maybe (HExp text filling)
plug (HExp t@(ICompose _ _ _) (hls,fhls)) i c | i `elem` hls = 
        do t' <- plugHoleI fillingToText t hls i c
           pure $ HExp t' (i `L.delete` hls,fhls)
plug _ _ _ = Nothing

-- | Plugs every hole in an expression with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- expression, then this function guarantees an expression with no holes (a constant).
plugAllI 
    :: Semigroup text
    => (filling -> text)
    -> [Natural]
    -> (Natural -> Maybe filling)  -- ^ Plug function.
    -> IHExp text              -- ^ IHExp to plug.
    -> Maybe (IHExp text)
plugAllI toText hls f (ICompose chk i r) | i `elem` hls = do
    chk' <- f i
    IChunk chk'' <- plugAllI toText hls f r
    return . IChunk $ chk <> toText chk' <> chk''
plugAllI _ _ _ (ICompose _ _ _) = Nothing
plugAllI _ _ _ t@(IChunk _) = return t

-- | Plugs every hole in an expression with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- expression, then this function guarantees an expression with no holes (a constant) is
-- returned.
plugAll :: HoleFilling text filling 
        => HExp text filling                     -- ^ HExp to plug
        -> ([Natural] -> (Natural -> Maybe filling)) -- ^ Plug function
        -> Maybe text
plugAll (HExp t (hls,fhls)) f | M.null fhls = 
    case plugAllI fillingToText hls (f hls) t of        
        Just (IChunk c) -> Just c
        _               -> Nothing
plugAll _ _ = Nothing

-- | In the simplest form, a type @filling@ is a hole filling if it
-- can be converted into @text@, because values of type @filling@ will
-- ultimately plug the hole they are filling. Optionally, a parser from t`Text`
-- into @filling@ can be declared as well. This makes it easier to plug a custom
-- parser in for @text@ making use of the existing parsers for the various
-- instances of t`HExp`.
class (Monoid text,Eq filling) => HoleFilling text filling  where    
    fillingToText :: filling -> text    

    parseFilling :: Maybe (Text -> Either Text filling)
    parseFilling = Nothing    

instance HoleFilling Text String where
    fillingToText :: String -> Text
    fillingToText = DT.pack

    parseFilling :: Maybe(Text -> Either Text String)
    parseFilling = Just $ Right . DT.unpack

instance HoleFilling String Text where
    fillingToText :: Text -> String
    fillingToText = DT.unpack

    parseFilling :: Maybe(Text -> Either Text Text)
    parseFilling = Just $ Right

-- | This class is used to define generic combinators on holey expressions. Simply, this
-- is the class of types that can be converted into a t`HExp`.
class HoleFilling text filling => ToHExp text filling a where
    toHExp :: a -> HExp text filling

-- | Used to add `HoleFilling` constraints to functions that don't take in an
-- explicit t`HExp`. This is useful for writing generic functions. 
data Proxy filling r = Proxy {
    runProxy :: r
}

instance (ToHExp text filling a) => ToHExp text filling (Either (HExp text filling) a) where
    toHExp :: Either (HExp text filling) a -> HExp text filling
    toHExp (Left t)  = t
    toHExp (Right a) = toHExp a

instance Monoid text => HoleFilling text () where
    fillingToText :: () -> text
    fillingToText () = mempty

-- | Translates a list into an expression list where each expression in the input
-- list is separated by the input expression.
sepHExpsBy :: (ToHExp text filling a)
           => HExp text filling   -- ^ Separator
           -> [a]                 -- ^ List of holey expressions
           -> HExp text filling
sepHExpsBy _   []     = chunk mempty
sepHExpsBy _   [v]    = toHExp v
sepHExpsBy sep (v:vs) = toHExp v +> sep +> sepHExpsBy sep vs 

-- | Add a prefix and suffix holey expressions to the given value.
betweenHExp :: (ToHExp text filling a) 
            => HExp text filling     -- ^ Prefix expression
            -> HExp text filling     -- ^ Suffice expression
            -> a                     -- ^ Value to be converted into an expression
            -> HExp text filling
betweenHExp b a (toHExp->t) = b +> t +> a
