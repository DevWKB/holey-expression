{-|
Module      : HExp
Description : Framework for creating text Holey Expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

-}
module  Data.HoleyExp.HExp (-- * Holey Expressions 
                            HExp
                           ,TextLike(..)
                           ,HoleFillingExp(..)
                           ,ToHExp(..)
                           -- ** Holes                            
                           ,Hole
                           ,HoleProps
                           -- *** Patterns
                           -- | Patterns make it easier to decide if a hole is empty, filled, or neither.
                           -- For example:
                           --
                           -- @
                           -- holeIndex :: Hole f -> Maybe Natural
                           -- holeIndex (EmptyHole i _)  = Just i
                           -- holeIndex (FilledHole i _) = Just i
                           -- holeIndex (UndefHole i _)  = Nothing
                           -- @
                           -- Each pattern uses the hole's properties
                           -- ('HoleProps') to decide if the hole's index is in
                           -- the required is location within the hole
                           -- properties, if not then it's considered
                           -- undefined. This prevents a lot of boilerplate
                           -- pattern matching.
                           ,pattern Empty
                           ,pattern Chunk
                           ,pattern Compose                           
                            -- ** Combinators
                            -- | The following combinators are the interface to
                            -- holey expressions. First, there are two
                            -- combinators for holes:
                            -- 
                            -- 1. Empty holes:
                            --
                            -- >>> hole 1
                            -- 
                            -- 2. Filled holes:
                            --
                            -- >>> filled 1 f
                            --
                            -- where @f@ is some hole filling of type @filling@.
                            -- There are no constraints on how many times a hole
                            -- index can occur. However, a hole is either filled
                            -- or empty, but not both.
                            --
                            -- Secondly, we have a combinator for chunks of                            
                            -- @text@:
                            --
                            -- >>> chunk t
                            --
                            -- where @t@ is some element of @text@.
                            -- 
                            -- Then we build larger expressions using
                            -- composition:
                            --
                            -- >>> e1 +> e2 +> ... +> ei
                            --
                            -- for some expressions @e1,e2,...,ei@. This
                            -- composition is associative, but non-commutative.
                           ,hole
                           ,filled
                           ,chunk
                           ,(+>)                                                      
                           -- *** Plugging Holes
                           -- | Holes can be either filled or plugged. The
                           -- former simply places a value of type @filling@
                           -- into the hole, but doesn't replace the hole. The
                           -- latter, replaces the hole altogether with the
                           -- value. There are two combinators for filling a
                           -- hole: a destructive one @fillHole@, and a
                           -- non-destructive one @placeInHole@. Finally,
                           -- @plugAll@ plugs every hole the function is defined for.
                           ,plugHole
                           ,plugAll
                           ,fillHole
                           ,placeInHole
                           -- *** Hole Properties
                           ,unfilledHoles
                           ,filledHoles
                           ,numberOfUnfilledHoles
                           ,numberOfFilledHoles
                           -- *** Equality
                           ,(==>)
                           -- *** Useful Helpers
                           ,showAST
                           ,sepHExpsBy
                           ,betweenHExp
                           ,chunkToText) where

import Data.HoleyExp.HExpInternal
