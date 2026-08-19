{-|
Module      : Template
Description : Generation of random templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate templates to be
used for property-based testing.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.QuickCheck.Template
    (genTemplate) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen
                                            ,Arbitrary (arbitrary)
                                            ,generate
                                            ,frequency
                                            ,sized)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Functor.Identity                (Identity)

import Data.TextTemplate.TemplateInternal
import Data.Text (Text)
import qualified Data.IntMap as M
import Data.Maybe (isJust, isNothing)
import Data.IntMap (keys, IntMap)

genChunk :: Arbitrary text => Gen (Template text filling)
genChunk = chunk <$> arbitrary

genHoleFilling :: Arbitrary filling => Gen (Maybe filling)
genHoleFilling @filling = sized $ \n -> 
    frequency
        [ (1, pure Nothing),
          (n, (arbitrary :: Gen filling) >>= (pure . Just))
        ]

genTemplateNat :: (Arbitrary text, Arbitrary filling) => Natural -> Gen (Template text filling)
genTemplateNat 0 = genChunk
genTemplateNat @text n = do (Template t holeProps) <- genTemplateNat $ n - 1
                            h <- arbitrary :: Gen Natural
                            f <- genHoleFilling
                            c <- arbitrary :: Gen text
                            let t' = ICompose c h t                      
                            pure $ Template t' $ holeProps `updateFreshHolePropsWith` (h,f)

genTemplate :: (Arbitrary text, Arbitrary filling) => Gen (Template text filling)
genTemplate = arbitrary >>= genTemplateNat 

instance (Arbitrary text, Arbitrary filling) => Arbitrary (Template text filling) where
    arbitrary :: Gen (Template text filling)
    arbitrary = genTemplate
