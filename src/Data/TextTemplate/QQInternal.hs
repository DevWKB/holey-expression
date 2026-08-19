{-|
Module      : QQInternal
Description : Quasi-Quoter for Templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Includes parsers for templates as well as a quasi-quoter 
for generating templates at compile time.
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.TextTemplate.QQInternal where

import GHC.Natural                        (Natural)
import Data.Text                          qualified as DT
import Data.Text                          (Text)
import Language.Haskell.TH.Quote          (QuasiQuoter(..))
import Language.Haskell.TH                qualified as TH
import Language.Haskell.TH                (Q
                                          ,Exp
                                          ,Name)
import Data.TextTemplate.TemplateInternal (Hole
                                          ,HoleProps
                                          ,Template(..)
                                          ,HoleFillingExp(hfExpToText, varHFExp)
                                          ,ITemplate(..)
                                          ,TextLike(toText)
                                          ,Proxy(..)
                                          ,pattern EmptyHole
                                          ,pattern FilledHole
                                          ,pattern UndefHole)
import Data.TextTemplate                  (parseTemplate)

-- | Text templates quasi quoter which contain `Text` filling.
textTemplate :: QuasiQuoter
textTemplate = QuasiQuoter {
    quoteExp  = textTemplate2QExp . Proxy @Text . DT.pack
   ,quotePat  = undefined
   ,quoteDec  = undefined
   ,quoteType = undefined
}

-- | Text templates quasi quoter which contain no (@Unit@) filling.
unitTemplate :: QuasiQuoter
unitTemplate = QuasiQuoter {
    quoteExp  = textTemplate2QExp . Proxy @() . DT.pack
   ,quotePat  = undefined
   ,quoteDec  = undefined
   ,quoteType = undefined
}

-- | Text templates quasi quoter which contain integer (@Int@) filling.
intTemplate :: QuasiQuoter
intTemplate = QuasiQuoter {
    quoteExp  = textTemplate2QExp . Proxy @Text . DT.pack
   ,quotePat  = undefined
   ,quoteDec  = undefined
   ,quoteType = undefined
}

--- | Parses a string into `Template Text` and then into a Template Haskell expression.
textTemplate2QExp :: HoleFillingExp Text filling
                  => Proxy filling Text    -- ^ String to parse as a textTemplate
                  -> Q Exp
textTemplate2QExp @filling =  (flip (.) (parseTemplate @filling . runProxy) $ \case {
         Right t  -> template2QExp t
        ;Left err -> fail $ DT.unpack err
    })

-- | Convert a `Hole` into a Template Haskell expression.
hole2QExp :: (TextLike text,HoleFillingExp text filling) => Proxy text (Hole filling) -> Q Exp
hole2QExp (Proxy (EmptyHole  i   _)) = appCombinator1 (TH.mkName "hole") (mkNaturalLit i)
hole2QExp @text (Proxy (FilledHole i f _)) = 
    appCombinator2 (TH.mkName "filled") (mkNaturalLit i) $        
        case varHFExp @text f of
            (Just v) -> TH.varE . TH.mkName . DT.unpack . toText $ v
            Nothing  -> TH.stringE . DT.unpack . toText $ hfExpToText @text f
hole2QExp (Proxy (UndefHole i _)) = error $ "QQ error: hole index "
                                  <> (show i)
                                  <> " present in internal textTemplate, but not defined in the templates hole properties."

-- | Convert an `ITemplate` into a Template Haskell expression.
iTemplate2QExp :: (TextLike text, HoleFillingExp text filling) 
               => ITemplate text 
               -> HoleProps filling 
               -> Q Exp
iTemplate2QExp (IChunk chk) _ = do
    let chunk = TH.mkName "chunk"
    appCombinator1 chunk $ mkTextLit . toText $ chk  
iTemplate2QExp @text @filling (ICompose p h r) hlsProps = do
    -- ICompose p h r = (chunk p) +> (hole h) +> r
    let pExp      = iTemplate2QExp (IChunk p) hlsProps
    let hExp      = hole2QExp (Proxy @text @(Hole filling) (h, hlsProps))
    let rExp      = iTemplate2QExp r hlsProps
    let compose   = appInfixCombinator (TH.mkName "+>")
    (pExp `compose` hExp) `compose` rExp

-- | Apply an infix combinator to a two arguments.
appInfixCombinator :: TH.Quote m 
                   => Name  -- ^ Name of the combinator
                   -> m Exp -- ^ First argument expression
                   -> m Exp -- ^ Second argument expression
                   -> m Exp 
appInfixCombinator constName e1 e2 = TH.infixE (Just e1) (TH.varE constName) (Just e2)

-- | Convert a type that can be converted into a textTemplate into a Template
-- Haskell expression. Use this to create new quasi-quoters for types that
-- convert to textTemplate.
template2QExp :: (TextLike text,HoleFillingExp text filling) => Template text filling -> Q Exp
template2QExp (Template it hls) = iTemplate2QExp it hls

-- * Helpful Template Haskell combinators.

-- | Apply a combinator to a single argument.
appCombinator1 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ Argument expression
               -> m Exp 
appCombinator1 constName = TH.appE (TH.varE constName) 

-- | Apply a combinator to two arguments.
appCombinator2 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ First argument expression
               -> m Exp -- ^ Second argument expression
               -> m Exp 
appCombinator2 constName a1 a2 = (TH.varE constName) `TH.appE`  a1 `TH.appE` a2 

-- | Apply a combinator to three arguments.
appCombinator3 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ First argument expression
               -> m Exp -- ^ Second argument expression
               -> m Exp -- ^ Third argument expression
               -> m Exp 
appCombinator3 constName a1 a2 a3 = (TH.varE constName) `TH.appE`  a1 `TH.appE` a2 `TH.appE` a3

-- | Convert a `Text` into a Template Haskell literal.
mkTextLit :: TH.Quote m 
          => DT.Text -- ^ Text to convert
          -> m Exp
mkTextLit = TH.litE . TH.StringL . DT.unpack

-- | Convert a `Natural` to a Template Haskell literal.
mkNaturalLit :: TH.Quote m 
            => Natural -- ^ Natural to convert
            -> m Exp
mkNaturalLit n | n >= 0 = TH.litE . TH.IntegerL . toInteger $ n
               | otherwise = error "QQ error: hole indices must be natural numbers"
