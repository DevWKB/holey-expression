{-|
Module      : TextTemplate
Description : Generation of random text templates
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate text templates to be
used for property-based testing.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.QuickCheck.TextTemplate
    (genTemplate) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen, Arbitrary (arbitrary), generate, frequency, sized)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Functor.Identity                (Identity)

import Data.TextTemplate.TemplateInternal
import Data.Text (Text)
import qualified Data.IntMap as M
import Data.Maybe (isJust, isNothing)
import Data.IntMap (keys, IntMap)

genChunk :: Gen (Template f)
genChunk = chunk <$> arbitrary

genHoleFilling :: Gen (Maybe Text)
genHoleFilling = sized $ \n -> 
    frequency
        [ (1, pure Nothing),
          (n, (arbitrary :: Gen Text) >>= (pure . Just))
        ]

genTemplateNat :: Natural -> Gen (Template Text)
genTemplateNat 0 = genChunk
genTemplateNat n = do (Template t holeProps) <- genTemplateNat $ n - 1
                      h <- arbitrary :: Gen Natural
                      f <- genHoleFilling
                      c <- arbitrary :: Gen Text
                      let t' = ICompose c h t                      
                      pure $ Template t' $ holeProps `updateFreshHolePropsWith` (h,f)

genTemplate :: Gen (Template Text)
genTemplate = arbitrary >>= genTemplateNat 

instance Arbitrary (Template Text) where
    arbitrary :: Gen (Template Text)
    arbitrary = genTemplate
