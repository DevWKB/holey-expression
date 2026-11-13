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
{-# LANGUAGE QuasiQuotes #-}
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
                                  )
import Data.NatMap                qualified as M
import Data.List.NonEmpty         (NonEmpty (..))
import Data.List.NonEmpty         qualified as NL
import Control.Lens               (Lens', lens, (^.))
import Control.Lens.Setter        qualified as L

-- | Internal properties of holes.
data HoleProps f
    = MkHProps {
     _emptyHolesHP :: [Natural] -- ^ Index list of all empty holes
    ,_filledHolesHP :: NatMap f -- ^ Map of all filled holes
    ,_pluggedHoleHP :: NatMap f -- ^ Map of all plugged holes
} 

-- | Lens for getting/setting the empty-holes hole property.
emptyHolesHP :: Lens' (HoleProps f) [Natural]
emptyHolesHP 
    = lens _emptyHolesHP 
    $ \(MkHProps _ fHP pHP ) eHP' 
        -> MkHProps eHP' fHP pHP

-- | Lens for getting/setting the filled-holes hole property.
filledHolesHP :: Lens' (HoleProps f) (NatMap f)
filledHolesHP 
    = lens _filledHolesHP
    $ \(MkHProps eHP _ pHP ) fHP' 
        -> MkHProps eHP fHP' pHP

-- | Lens for getting/setting the plugged-holes hole property.
pluggedHolesHP :: Lens' (HoleProps f) (NatMap f)
pluggedHolesHP 
    = lens _pluggedHoleHP
    $ \(MkHProps eHP fHP _ ) pHP'
        -> MkHProps eHP fHP pHP'

-- | Accessing setter of an 'HoleProps'.
(%~) :: Lens' (HoleProps f) a -> (a -> a) -> (HoleProps f -> HoleProps f)
(%~) l f = (L.%~) l f

-- | Predicate that returns 'True' when the input 'HoleProps' is empty.
isEmptyHProps :: HoleProps f -> Bool
isEmptyHProps hprops 
    = L.null (hprops ^. emptyHolesHP)
   && M.null (hprops ^. filledHolesHP)
   && M.null (hprops ^. pluggedHolesHP)

-- | Union two `HoleProps`.
union :: HoleProps f -> HoleProps f -> HoleProps f
union hprops1 hprops2 
    = emptyHolesHP   %~ (L.union $ hprops2 ^. emptyHolesHP) 
    $ filledHolesHP  %~ (M.union $ hprops2 ^. filledHolesHP) 
    $ pluggedHolesHP %~ (M.union $ hprops2 ^. pluggedHolesHP) 
    $ hprops1

-- | Equality for 'HoleProps'.
eqHProps :: Eq f => HoleProps f -> HoleProps f -> Bool
eqHProps hprops1 hprops2 
    = (hprops1 ^. emptyHolesHP)   == (hprops2 ^. emptyHolesHP) 
   && (hprops1 ^. filledHolesHP)  == (hprops2 ^. filledHolesHP) 
   && (hprops1 ^. pluggedHolesHP) == (hprops2 ^. pluggedHolesHP) 

-- | Moves an empty hole to a filled hole.
emptyHole2Filled 
    :: HoleProps f -- ^ Hole props to update
    -> Natural       -- ^ Index of the empty hole
    -> f             -- ^ Filling
    -> HoleProps f
emptyHole2Filled hprops hindex filling 
    = emptyHolesHP  %~ (L.delete hindex)
    $ filledHolesHP %~ (insert hindex filling)
    $ hprops

-- | The opposite of 'emptyHole2Filled'.
filledHole2Empty 
    :: HoleProps f
    -> Natural
    -> HoleProps f
filledHole2Empty hprops hindex
    = emptyHolesHP  %~ (L.insert hindex) 
    $ filledHolesHP %~ (delete hindex)
    $ hprops

-- | Moves an empty hole to a plugged hole.
emptyHole2Plugged 
    :: Natural     -- ^ Index of the empty hole
    -> f           -- ^ Filling
    -> HoleProps f
    -> HoleProps f
emptyHole2Plugged hindex filling hprops
    = emptyHolesHP  %~ (L.delete hindex)
    $ pluggedHolesHP %~ (insert hindex filling)
    $ hprops

-- | Moves an empty hole to a plugged hole.
filledHole2Plugged 
    :: Natural     -- ^ Index of the empty hole
    -> HoleProps f
    -> HoleProps f
filledHole2Plugged hindex hprops
    = let filling_ = (hprops ^. filledHolesHP) !? hindex
       in maybe 
            hprops
            (\filling -> 
                  filledHolesHP  %~ (M.delete hindex)
                $ pluggedHolesHP %~ (insert hindex filling)
                $ hprops)
            filling_

-- | A hole that can be filled with a filling of type @f@ is essentially a
-- natural number and a set of hole properties (t`HoleProps`).
type Hole f = (Natural,HoleProps f)

-- ** Hole Patterns

-- | Pattern synonym for empty holes. 
pattern EmptyHole :: Hole f
pattern EmptyHole <- (decomposeEmptyHole -> True)

-- | Determines if the input hole index @i@ is the index of an empty hole.
decomposeEmptyHole :: (Natural, HoleProps f) -> Bool
decomposeEmptyHole (i,hprops) = i `elem` hprops ^. emptyHolesHP                                  

-- | Pattern synonym for filled holes.
pattern FilledHole :: f -> Hole f
pattern FilledHole f <- (decomposeFilledHole -> Just f)

-- | Determines if the input hole index @i@ is the index of a filled hole.
decomposeFilledHole :: Hole f -> Maybe f
decomposeFilledHole (i,hprops) = (hprops ^. filledHolesHP) !? i

-- | Pattern synonym for filled holes.
pattern PluggedHole :: f -> Hole f
pattern PluggedHole t <- (decomposePluggedHole -> Just t)

-- | Determines if the input hole index @i@ is the index of a plugged hole.
decomposePluggedHole :: Hole f -> Maybe f
decomposePluggedHole (i,hprops) = (hprops ^. pluggedHolesHP) !? i

-- | Pattern synonym for undefined holes. These are holes which are not currently used
-- in the expression; and thus, are neither free nor empty.
pattern UndefHole :: Hole f
pattern UndefHole <- (decomposeUndefHole -> True)

-- | Determines if the input hole index @i@ is neither an empty hole or a filled
-- hole; thus, is undefined in the expression.
decomposeUndefHole :: Hole f -> Bool
decomposeUndefHole h 
    = not (decomposeEmptyHole h) 
   && isNothing (decomposeFilledHole h) 
   && isNothing (decomposePluggedHole h)

{-# COMPLETE EmptyHole, FilledHole, PluggedHole, UndefHole #-}

-- | Tests to see if a hole index exist in the given hole properties.
isFreshHoleIndex :: Natural       -- ^ Hole index
                 -> HoleProps f -- ^ Hole properties
                 -> Bool
isFreshHoleIndex h holeProps 
    = not 
    $ h `elem` (holeProps ^. emptyHolesHP) 
   && h `elem` (keys $ holeProps ^. filledHolesHP) 
   && h `elem` (keys $ holeProps ^. pluggedHolesHP) 

-- | The hole properties with no defined holes.
emptyHoleProps :: HoleProps f
emptyHoleProps = MkHProps [] M.empty M.empty

-- | The underlying structure of t'HExp'.
data IHExp text where
    IChunk   :: text -> IHExp text
    ICompose :: text -> NonEmpty Natural -> IHExp text -> IHExp text
    deriving Show

-- | An expression with pluggable holes. We do not expose the underlying
-- constructors in favor of the combinators.
data HExp text filling where
    HExp :: IHExp text        -- ^ Internal expression
         -> HoleProps filling -- ^ Empty holes and hole-filling map
         -> HExp text filling

instance (TextLike text, HoleFilling text filling) 
    => Show (HExp text filling) where
    show :: HExp text filling -> String    
    show (HExp (IChunk t) _) = DT.unpack . toText $ t    
    show (HExp (ICompose prefix holeIndexList rest) hprops)
        = (DT.unpack . toText $ prefix)     
        <> (foldl (\acc hindex -> 
                    case (hindex,hprops) of
                        EmptyHole     -> acc <> mkHoleText hindex Nothing
                        FilledHole  _ -> acc <> (mkHoleText hindex $ (hprops ^. filledHolesHP) !? hindex) <> ")"
                        PluggedHole _ -> fToS $ (hprops ^. pluggedHolesHP) ! hindex
                        UndefHole     -> error "unrecognized hole index: " <> show hindex) 
                  "" 
                  holeIndexList)
        <> show (HExp rest hprops)
        where
            fToT :: filling -> text
            fToT = fillingToText

            fToS = DT.unpack . toText . fToT

            mkHoleText :: Natural -> Maybe filling -> String
            mkHoleText hindex filling
                = "$" <> show hindex 
                      <> "("
                      <> maybe "" fToS filling 
                      <> ")"            
-- * Combinators

-- | Pattern synonym for the empty expression.
pattern Empty :: (Eq text, Monoid text) => HExp text filling
pattern Empty <- (null -> True) where
    Empty = emptyExp

-- | Decides if an expression corresponds to a chunk or not. 
isChunk :: HExp text filling -> Maybe text
isChunk (HExp (IChunk s) hprops) | isEmptyHProps hprops = Just s
isChunk _ = Nothing

-- | Pattern synonym for expression chunk's.
pattern Chunk :: text -> HExp text filling
pattern Chunk s <- (isChunk -> Just s)
    where
        Chunk = chunk

-- | Construct an `HExp` from a hole-index list.
fromHoleIndexList 
    :: HoleFilling t f
    => NonEmpty Natural 
    -> HoleProps f 
    -> HExp t f
fromHoleIndexList h hprops 
    = foldl (\acc hindex -> 
                acc +> 
                    case (hindex,hprops) of 
                        EmptyHole     -> empty hindex
                        FilledHole f  -> filled hindex f
                        PluggedHole t -> chunk . fillingToText $ t
                        UndefHole     -> error $ "unrecognized hole index: "<>show hindex) 
            emptyExp 
            h

-- | Pattern synonym for the composition of holey expressions.
pattern Compose :: (Monoid text, Eq text) 
    => text 
    -> NonEmpty Natural
    -> HExp text filling 
    -> HExp text filling
pattern Compose c h t <- (decompose -> Just (c, h, t))

{-# COMPLETE Chunk, Compose #-}

{-# DEPRECATED compose "not very useful since switching to representing hole indexes using lists." #-}
-- | Explicitly create a top-level composition expression.
compose :: (Monoid text, Eq text)
        => text                   -- ^ Prefix chunk
        -> (Natural,Maybe filling)           
        -> HExp text filling  -- ^ HExp branch
        -> HExp text filling
compose c (i, Nothing) t = chunk c +> empty i    +> t
compose c (i, Just f)  t = chunk c +> filled i f +> t

-- | Decompose an expression into the top-level compose.
decompose :: HExp text filling 
          -> Maybe (text, NonEmpty Natural, HExp text filling)
decompose (HExp (ICompose c i t') hprops) = Just (c,i,HExp t' hprops)
decompose _ = Nothing

-- | Decide if an element of a monoid is the unit.
isUnit :: (Eq m, Monoid m) 
        => m 
        -> Bool
isUnit m | m == mempty = True
         | otherwise   = False

-- | Test to see if an expression is empty.
null :: (Eq text, Monoid text) => HExp text filling -> Bool
null (HExp (IChunk c) hprops) 
    = isUnit c && isEmptyHProps hprops
null _ 
    = False

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
(HExp t1 hprops1) ==> (HExp t2 hprops2) 
    = t1 >==> t2 && (hprops1 `eqHProps` hprops2)

-- | An empty hole.
empty :: Monoid text
     => Natural -- ^ Hole index
     -> HExp text filling
empty i 
    = HExp (ICompose mempty (NL.singleton i) (IChunk mempty))
    $ emptyHolesHP %~ (i:) $ emptyHoleProps

-- | A hole with a filling. 
filled :: Monoid text
       => Natural -- ^ Hole index
       -> filling -- ^ Hole filling
       -> HExp text filling
filled i f 
    = HExp (ICompose mempty (NL.singleton i) (IChunk mempty))
    $ filledHolesHP %~ (insert i f) $ emptyHoleProps

-- | A chunk is a constant; it's helpful to think of these as a piece of
-- subtext. 
chunk :: text -- ^ Constant
      -> HExp text filling
chunk = flip HExp emptyHoleProps .  IChunk

-- | The empty expression.
emptyExp :: Monoid text 
         => HExp text filling
emptyExp = chunk mempty                                                     

-- | Composition of `IHExp`.
(>+>) :: (Monoid text, Eq text)
      => IHExp text 
      -> IHExp text 
      -> HoleProps fext
      -> IHExp text 
(>+>) (IChunk chk1) (IChunk chk2) _   
    = IChunk $ chk1 <> chk2
(>+>) (IChunk chk)  (ICompose p h r) _ 
    = ICompose (chk <> p) h r
(>+>) (ICompose p h r) t hprops 
    = case r >+> t $ hprops of
        ICompose p' h' t' 
         | p' == mempty -> ICompose p (NL.append h h') t'
        exp -> ICompose p h exp

-- | Composition of holey expressions.
(+>) :: (Monoid text, Eq text )
     => HExp text filling
     -> HExp text filling
     -> HExp text filling
(HExp t1 hprops1) +> (HExp t2 hprops2) 
    = let hprops = hprops1 `union` hprops2
       in HExp (t1 >+> t2 $ hprops) hprops

instance (Monoid text, Eq text) => Semigroup (HExp text filling) where
    (<>) :: HExp text filling -> HExp text filling -> HExp text filling
    (<>) = (+>)

instance (Monoid text, Eq text) => Monoid (HExp text filling) where
    mempty :: HExp text filling
    mempty = emptyExp

    mconcat :: [HExp text filling] -> HExp text filling
    mconcat = foldr (<>) emptyExp

instance Functor (HExp text) where
    fmap :: (filling1 -> filling2) -> HExp text filling1 -> HExp text filling2
    fmap f (HExp t (MkHProps eHP fHP pHP)) 
        = HExp t $ MkHProps { 
           _emptyHolesHP  = eHP
          ,_filledHolesHP = (M.map f fHP)
          ,_pluggedHoleHP = (M.map f pHP)
        }

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

instance TextLike () where
    toText :: () -> Text
    toText = DT.show

{-
-- | Convert a holey expression's AST into a `Text`. The `Show` instance for
-- t`HExp` is set to pretty print, but for debugging it is sometimes useful to
-- see the raw AST.
showAST :: (TextLike text, TextLike filling) => HExp text filling -> Text
showAST (HExp (IChunk x) _)                  = "IChunk "   
                                            <> quotes (toText x)
showAST (HExp (ICompose p i r) hls@(_,fhls)) = "ICompose " 
                                            <> quotes (toText p) 
                                            <> " " 
                                            <> showHole i hls 
                                            <> " " 
                                            <> parens (showAST (HExp r hls)) 
    where
        showHole :: NonEmpty Natural -> HoleProps filling -> Text
        showHole i hls | emptyHole i hls = DT.show . NE.toList $ i
                       | not (emptyHole i hls) && NE.length i == 1 = let h = NE.head i 
                                                                         f = toText $ fhls ! h
                                                                      in parens $ (DT.show h) <> ", " <> f
                       | otherwise = error 
                                   $ "[showAST] hole index list "
                                  <> (show i)
                                  <> " contains one or more index with one of them being filled."

-- | Prints the AST of an expression to the terminal. This is useful for debugging.
printAST :: (TextLike text, TextLike filling) => HExp text filling -> IO ()
printAST = putStrLn . DT.unpack . showAST 
-}

-- | Get the list of empty-hole indices present in an expression.
-- Time complexity: \( \mathcal{O}(0) \)
unfilledHoles :: HExp text filling -- ^ HExp 
              -> [Natural]
unfilledHoles (HExp _ hprops) = hprops ^. emptyHolesHP

-- | Get the list of filled-hole indices present in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
filledHoles :: HExp text filling -- ^ HExp 
            -> [Natural]
filledHoles (HExp _ hprops) = keys $ hprops ^. filledHolesHP

-- | Get the list of plugged-hole indices present in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
pluggedHoles :: HExp text filling -- ^ HExp 
             -> [Natural]
pluggedHoles (HExp _ hprops) = keys $ hprops ^. pluggedHolesHP

-- | Get the filling of a hole. Returns @Nothing@ when the hole doesn't exist.
fillingInHole :: HExp text filling -- ^ HExp
              -> Natural           -- ^ Hole index
              -> Maybe filling
fillingInHole (HExp _ hprops) h 
    = (hprops ^. filledHolesHP) 
   !? h

-- | Get the text that is plugging a hole.
-- Returns 'Nothing' when the hole doesn't exist.
holePlug :: HExp text filling
         -> Natural
         -> Maybe filling
holePlug (HExp _ hprops) h
    = (hprops ^. pluggedHolesHP)
   !? h

{-# DEPRECATED numberOfUnfilledHoles "renamed to numberOfEmptyHoles." #-}
-- | Get the number of unfilled holes in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
numberOfUnfilledHoles 
    :: HExp text filling -- ^ HExp 
    -> Int
numberOfUnfilledHoles = numberOfEmptyHoles

-- | Get the number of unfilled holes in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
numberOfEmptyHoles :: HExp text filling -- ^ HExp 
                      -> Int
numberOfEmptyHoles (HExp _ hprops) 
    = length $ hprops ^. emptyHolesHP

-- | Get the number of filled holes in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
numberOfFilledHoles :: HExp text filling -- ^ HExp 
                    -> Int
numberOfFilledHoles (HExp _ hprops) 
    = M.size $ hprops ^. filledHolesHP

-- | Get the number of plugged holes in an expression.
-- Time complexity: \( \mathcal{O}(n) \)
numberOfPluggedHoles 
    :: HExp text filling -- ^ HExp 
    -> Int
numberOfPluggedHoles (HExp _ hprops) 
    = M.size $ hprops ^. pluggedHolesHP

-- | Decide if an expression is filled or not. 
-- Time complexity: \(\mathcal{O}(n)\)
isFilled :: HExp text filling -> Bool
isFilled t = numberOfEmptyHoles t == 0

-- | Convert an expression with no holes, a chunk, into a text.
-- Time complexity: \( \mathcal{O}(0) \)
chunkToText :: HExp text filling 
            -> Maybe text
chunkToText (HExp (IChunk c) hprops) | isEmptyHProps hprops 
    = Just c
chunkToText _                                         
    = Nothing

-- | Like `update`, but doesn't update an already filled hole's value.
place :: HExp text filling
            -> Natural -- ^ Hole index to plug
            -> filling -- ^ Hole filling
            -> Maybe (HExp text filling)
place t@(HExp it hprops) i c =
    case (i,hprops) of
        EmptyHole     -> Just . HExp it $ emptyHole2Filled hprops i c
        FilledHole _  -> Just t
        PluggedHole _ -> Nothing -- Pretend we didn't find the index.
        UndefHole     -> Nothing


-- | Update a hole adding or removing a filling. If the hole is already filled,
-- then the filling is updated with the new value. Filling a hole doesn't
-- replace the hole, but simply puts the input @filling@ inside the hole.
-- Returns @Nothing@ if the hole doesn't exist or has already been filled. If
-- the input filling is `Nothing`, then the hole is emptied. The complexity of
-- this operation is \(\mathcal{O}(\max(n_0,\min(n_1,W)))\), where \(n_0\) is
-- the number of empty holes, and \(n_1\) is the number of filled holes with a
-- max of \(W\) the number of bits in an `Int` (32 or 64).
update :: HExp text filling
             -> Natural       -- ^ Hole index to fill
             -> Maybe filling -- ^ Hole filling
             -> Maybe (HExp text filling)
update (HExp t hprops) i Nothing = 
    case (i,hprops) of
        EmptyHole     -> Just $ HExp t hprops
        FilledHole  _ -> Just . HExp t $ filledHole2Empty hprops i
        PluggedHole _ -> Nothing
        UndefHole     -> Nothing
update (HExp t hprops) i (Just c) = 
    case (i,hprops) of
        EmptyHole     -> Just . HExp t $ emptyHole2Filled hprops i c
        FilledHole _  -> Just . HExp t $ filledHolesHP %~ (insert i c) $ hprops
        PluggedHole _ -> Nothing
        UndefHole     -> Nothing

-- | Case split over a 'Hole f'.
caseHole 
    :: Hole f
    -> r          -- ^ Undefined hole
    -> r          -- ^ Empty hole
    -> (f -> r)   -- ^ Filled hole
    -> (f -> r)   -- ^ Plugged hole
    -> r
caseHole (i,hprops) onUndef onEmpty onFilled onPlugged
    = case (i,hprops) of
        EmptyHole     -> onEmpty
        FilledHole f  -> onFilled f
        PluggedHole f -> onPlugged f
        UndefHole     -> onUndef

-- | Plug an empty hole in an expression with some filling. Returns @Nothing@ when
-- the hole index doesn't exist in the expression or is filled, otherwise returns
-- an expression with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `update`.
plug 
    :: HExp text filling
    -> Natural       -- ^ Hole index to fill
    -> filling       -- ^ Hole filling
    -> Maybe (HExp text filling)
plug (HExp t hprops) i c = 
    caseHole (i,hprops)
        Nothing
        (Just . HExp t $ emptyHole2Plugged i c hprops)
        (const Nothing)
        (const Nothing)

-- | Plug a filled hole with its filling (committing to the filling). 
-- Returns 'Nothing' if the hole index is empty or undefined.
commit
    :: HExp text filling
    -> Natural
    -> Maybe (HExp text filling)
commit (HExp e hprops) i = 
    caseHole (i,hprops)
        Nothing
        Nothing
        (\f -> Just . HExp e $ filledHole2Plugged i hprops)        
        (const Nothing)

-- | Plugs every hole in an expression with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- expression, then this function guarantees every hole is plugged.
plugAll 
    :: HExp text filling                         -- ^ HExp to plug
    -> ([Natural] -> (Natural -> Maybe filling)) -- ^ Plug function
    -> Maybe (HExp text filling)
plugAll (HExp e hprops) plugFun' 
    | M.null (hprops ^. filledHolesHP)
        = do let pf acc hindex 
                    = do hprops' <- acc
                         filling <- plugFun hindex
                         pure $ emptyHole2Plugged hindex filling hprops'
             hprops' <- foldl pf (Just hprops) eHP
             pure $ HExp e hprops'
    | otherwise = Nothing
    where
        eHP = hprops ^. emptyHolesHP
        plugFun = plugFun' eHP

-- | Commits every filled hole in the given expression. 
-- Empty holes are left untouched.
commitAll 
    :: HExp text filling -- ^ HExp to commit
    -> HExp text filling
commitAll (HExp e hprops) 
    = let pf acc hindex = filledHole2Plugged hindex acc
       in HExp e $ foldl pf hprops fHP
    where
        fHP = keys (hprops ^. filledHolesHP)

-- | In the simplest form, a type @filling@ is a hole filling if it
-- can be converted into @text@, because values of type @filling@ will
-- ultimately plug the hole they are filling. Optionally, a parser from t`Text`
-- into @filling@ can be declared as well. This makes it easier to plug a custom
-- parser in for @text@ making use of the existing parsers for the various
-- instances of t`HExp`.
class (Monoid text,Eq text,Eq filling) => HoleFilling text filling  where    
    fillingToText :: filling -> text    

    parseFilling :: Maybe (Text -> Either Text filling)
    parseFilling = Nothing    

instance HoleFilling Text Text where
    fillingToText :: Text -> Text
    fillingToText = id

    parseFilling :: Maybe(Text -> Either Text Text)
    parseFilling = Just Right

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

instance HoleFilling Text Integer where
    fillingToText :: Integer -> Text
    fillingToText = DT.show

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

instance (Monoid text, Eq text) => HoleFilling text () where
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
