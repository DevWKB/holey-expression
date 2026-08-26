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
\(\mathsf{Fill}\) which we call its elements __fillings__. 

We define the collection of __holey expressions__ to be:

\( \mathsf{HExp}(\mathsf{Txt},\mathsf{Fill}) = \Pi(\mathsf{Txt} + (\mathbb{N} \times \mathsf{Fill}_{\mathsf{?}}))  \)

We call elements of \( \mathbb{N} \times \mathsf{Fill}_{\mathsf{?}} \)
__holes__, and denote __filled holes__ by \($(i,f)\) and __empty holes__ by
\($(i,\mathsf{?})\). Lastly, composition of expressions, elements of
\(\mathsf{HExp}\), is concatenation of products denoted by 
\(e_1 * \cdots * e_i \); note that we leave injections implicit to make the expression more readable.

Let's consider a few abstract example expressions:

1. \( t_1 * $(1,\mathsf{?}) * t_2 * $(2,\mathsf{?}) \)
2. \( t_1 * $(5,f_5) * $(3,\mathsf{?}) * t_2 * $(7,f_7) \)

Now if we choose some concrete sets for \(\mathsf{Txt}\) and \(\mathsf{Fill}\)
then we can create more interesting expressions:

1. \(123 * $(1,?) * 456 * $(2,5) : \mathsf{HExp}(\mathbb{N},\mathbb{N})\), where
    \((\mathbb{N},0,+)\) is the monoid for \(\mathsf{Txt}\)
2. \(\text{"Hi, my name is "} * $(1,?) : \mathsf{HExp}(\Sigma^*,\Sigma^*)\), where
   \((\Sigma^*,\circ)\) is the monoid of words over the English alphabet.
3. \($(1,?) * \text{":"} * $(2,?) * \text{":"} * $(3,?) : \mathsf{HExp}(\Sigma^*,\mathbb{N})\), where
   \((\Sigma^*,\circ)\) is the monoid of words over \(\Sigma = \mathbb{N} \cup \{\text{":"}\}\). 
    This could represent time.

Holey expressions are ultimately meant to be translated into \(\mathsf{Txt}\) by
filling in all of their holes. This implies that we must require the existence
of a function \(p : \mathsf{Fill} \to \mathsf{Txt} \). There are two operations
on holes: i. plugging a hole (replacing it with a filling) and ii. filling a
hole (placing a filling inside the hole). 

Plugging a hole amounts to defining a function 
\(\mathsf{plug} : \mathbb{N} \times \mathsf{Fill}_\mathsf{?} \to \mathsf{Fill}_\perp\)
that chooses which filling to replace the hole with; note that this is a partial
function, and is defined per-expression. Then plugging an expression corresponds to
the function: 
\( \Pi(\mathsf{id} + (\mathsf{plug};p_\perp)) : \mathsf{HExp}(\mathsf{Txt},\mathsf{Fill}) \to \mathsf{Txt}_\perp \).
If the plug function is defined for all holes in the input expression, then the
above composition will indeed yield a chunk (an element of \(\mathsf{Txt}\)).

Filling a hole is a bit more simple, and requires the definition of a function
\(\mathsf{place} : \mathsf{Fill}_\mathsf{?} \to \mathsf{Fill}_\mathsf{?}\)
that simply updates the filling in the hole. Then filling an expression corresponds to
the function: 
\( \Pi(\mathsf{id} + (\mathsf{id} \times \mathsf{place})) : \mathsf{HExp}(\mathsf{Txt},\mathsf{Fill}) \to \mathsf{HExp}(\mathsf{Txt},\mathsf{Fill}) \).
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
