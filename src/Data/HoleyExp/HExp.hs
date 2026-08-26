{-|
Module      : HExp
Description : Holey Expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Holey expressions correspond to a monoid with two kinds of elements: i. chunks
and ii. holes. The former correspond to chunks of "text" which we leave
abstract, and the latter correspond to placeholders for values that will
eventually be translated into "text".

Suppose we have a monoid \((\mathsf{Txt},\otimes,\mathsf{e})\), where we call
elements of \(\mathsf{Txt}\) __chunks__. Furthermore, suppose we have a set
\(\mathsf{Fill}\) which we call its elements __fillings__. Then we define a
__hole__ as a function \(h : \mathbb{N} \times \mathsf{Fill}_{\perp} \to \mathsf{Txt}\).

We define a __holey expression__ to be a function:

\( \mathop{exp} : \Pi_{i \in \mathbb{N}}(\mathbb{N},\mathsf{Fill}) \to \mathsf{Txt}  \)

Let's consider several example expressions:

1. \(\mathop{exp}_2((1,\perp),(2,\perp)) = t_1 \otimes h(1,\perp) \otimes t_2   \otimes h(2,\perp)\)

\( t_1 * h(2) = \lambda f.t_1 \otimes h(2,f)  \)

\( t_1 * h(1,f_1) = \lambda f_\perp.t_1 \otimes h(2,if\,f = \perp\,then\,f_1\,else\,f)  \)

-}
module  Data.HoleyExp.HExp (-- * Holey Expressions 
                            HExp
                           ,TextLike(..)
                           ,HoleFillingExp(..)
                           ,ToHExp(..)
                           -- __ Holes                            
                           ,Hole
                           ,HoleProps
                           -- __* Patterns
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
                            -- __ Combinators
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
                           -- __* Plugging Holes
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
                           -- __* Hole Properties
                           ,unfilledHoles
                           ,filledHoles
                           ,numberOfUnfilledHoles
                           ,numberOfFilledHoles
                           -- __* Equality
                           ,(==>)
                           -- __* Useful Helpers
                           ,showAST
                           ,sepHExpsBy
                           ,betweenHExp
                           ,chunkToText) where

import Data.HoleyExp.HExpInternal
