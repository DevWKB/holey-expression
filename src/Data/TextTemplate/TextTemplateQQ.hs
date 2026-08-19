{-|
Module      : TextTemplateQQ
Description : Quasi-quoters for Text Templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

-}
module Data.TextTemplate.TextTemplateQQ 
    (-- * Quasi-Quoter for Templates
     textTemplate
    ,unitTemplate
    ,intTemplate
    ,textTemplate2QExp
    ,template2QExp) where

import Data.TextTemplate.QQInternal 
