{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeApplications       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : TextTemplateQQ
Description : Quasi-quoters for Text Templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

This is the quasi-quoter for "Data.TextTemplate"; see its documentation for an
introduction.

Using the quasi-quoter we can write templates as their own expressions.

For example, 

>>> let textDoubleTemplate = runProxy @Double textTemplate
>>> let t = [textDoubleTemplate|Today Temperature: $1{} high/$2{} low|] :: Template Text Double
>>> t
Today Temperature: $1{} high/$2{} low

Here is an example with a filled hole:

>>> let t' = [textDoubleTemplate|Today\'s Temperature: $1{} high/$2{77.3} low|] :: Template Text Double
>>> t'
Today's Temperature: $1{} high/$2{77.3} low

-}
module Data.TextTemplate.TextTemplateQQ 
    (-- * Text Templates
     module Data.TextTemplate
     -- * Quasi-Quoter for Text Templates
    ,textTemplate
    ,textTemplate2QExp
    ,template2QExp
    ,unitTemplateQQ
    ,textTemplateQQ) where

import Data.TextTemplate
import Data.TextTemplate.TemplateInternal (runProxy)
import Data.TextTemplate.QQInternal 
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH (Q, Exp)
import qualified Language.Haskell.TH as TH

unitTemplateQQ :: QuasiQuoter
unitTemplateQQ = runProxy @() textTemplate

textTemplateQQ :: QuasiQuoter
textTemplateQQ = runProxy @Text textTemplate

instance ToQExp () where
    toQExp :: () -> Q Exp
    toQExp () = TH.conE . TH.mkName $ "()"

instance TemplateQExp Text ()
instance TemplateQExp Text Text
