{-|
Module      : Template
Description : The combinator library of templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

The base combinator library for templates. 

Write this from the perspective of monoids.
-}
module  Data.TextTemplate.Template 
                           (-- * Templates 
                            Template
                            ,TextLike(..)
                            ,HoleFillingExp(..)
                            ,ToTemplate(..)
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
                            -- ** Template Combinators
                           ,hole
                           ,filled
                           ,chunk
                           ,(+>)                                                      
                           -- *** Plugging Holes in Templates
                           ,plugHole
                           ,plugAll
                           ,fillHole
                           ,placeInHole
                           -- *** Template Hole Properties
                           ,unfilledHoles
                           ,filledHoles
                           ,numberOfUnfilledHoles
                           ,numberOfFilledHoles
                           -- *** Equality
                           ,(==>)
                           -- *** Useful Helpers
                           ,showAST
                           ,sepTemplatesBy 
                           ,betweenTemplate
                           ,chunkToText) where

import Data.TextTemplate.TemplateInternal
