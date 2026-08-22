{-|
Module      : HExp
Description : Framework for creating text Holey Expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.me

Holey expressions are essentially monoids with two types of elements "chunks of
text" (constants) and "holes" (placeholders for constants). Then when all holes 
are plugged in an expression it corresponds to a piece of text; here we are using 
@text@ abstractly, and in fact, the base definition of `HExp`'s is abstract in 
both the type of text as well as the type of values you can fill holes with.

A simple example:

>>> let t = (chunk "Today's Temperature: ") +> (hole 1) +> (chunk " high/") +> (hole 2) +> (chunk " low") :: HExp Text Double
>>> t
Today's Temperature: $1{} high/$2{} low

>>> plugAll t $ \_ -> \i -> if i == 1 then Just 91.2 else if i == 2 then Just 87.0 else Nothing 
Just "Today's Temperature: 91.2 high/87.0 low"

The above is an example of an expression of type @HExp Text Double@ where
the first type is the type of constants which is what we are ultimately constructing a value of when all
holes are plugged, and the second type is the type of the filling we place in
the holes.

A second way we can write the same expression using @OverloadedStrings@ is:

>>> let t'' = "Today's Temperature: " <> (hole 1) <> " high/" <> (hole 2) <> " low" :: HExp Text Double
>>> t ==> t''
True

There is a third way as well, but it requires HExp Haskell; see
"Data.HoleyExp.HExpQQ".
-}
module  Data.HoleyExp.HExp 
                           (-- * Holey Expressions 
                            HExp
                            ,TextLike(..)
                            ,HoleFillingExp(..)
                            ,ToHExp(..)
                            -- ** Holes                            
                           ,Hole
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
                           ,hole
                           ,filled
                           ,chunk
                           ,(+>)                                                      
                           -- *** Plugging Holes
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
