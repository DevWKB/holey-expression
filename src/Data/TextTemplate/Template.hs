{-|
Module      : Template
Description : The combinator library of templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

This is the combinator library for plain templates which do not specify the
types of their text or hole filling.
-}
module  Data.TextTemplate.Template 
                           (-- * Templates 
                            Template
                           ,pattern Empty
                           ,pattern Chunk
                           ,pattern Compose
                           ,Hole
                            -- ** Template Combinators
                           ,hole
                           ,filled
                           ,chunk
                           ,(+>)
                           ,showAST
                           ,sepTemplatesBy 
                           ,betweenTemplate
                           -- ** Template Properties
                           ,unfilledHoles
                           ,filledHoles
                           ,numberOfUnfilledHoles
                           ,numberOfFilledHoles                        
                           -- ** Plugging Holes in Templates
                           ,plugHole
                           ,plugAll
                           -- ** Equality and Matching
                           ,(==>)
                           -- ** Converting from Templates
                           ,chunkToText                           
                           ) where

import Data.TextTemplate.TemplateInternal
