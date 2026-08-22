{-|
Module      : HExp
Description : Generation of random holey expressions
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate holey expressions to be
used for property-based testing.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.QuickCheck.HExp
    (genHExp) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen
                                            ,Arbitrary (arbitrary)
                                            ,generate
                                            ,frequency
                                            ,sized)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Functor.Identity                (Identity)

import Data.HoleyExp.HExpInternal
import Data.Text (Text)
import qualified Data.IntMap as M
import Data.Maybe (isJust, isNothing)
import Data.IntMap (keys, IntMap)

genChunk :: Arbitrary text => Gen (HExp text filling)
genChunk = chunk <$> arbitrary

genHoleFilling :: Arbitrary filling => Gen (Maybe filling)
genHoleFilling @filling = sized $ \n -> 
    frequency
        [ (1, pure Nothing),
          (n, (arbitrary :: Gen filling) >>= (pure . Just))
        ]

genHExpNat :: (Arbitrary text, Arbitrary filling) => Natural -> Gen (HExp text filling)
genHExpNat 0 = genChunk
genHExpNat @text n = do (HExp t holeProps) <- genHExpNat $ n - 1
                        h <- arbitrary :: Gen Natural
                        f <- genHoleFilling
                        c <- arbitrary :: Gen text
                        let t' = ICompose c h t                      
                        pure $ HExp t' $ holeProps `updateFreshHolePropsWith` (h,f)

genHExp :: (Arbitrary text, Arbitrary filling) => Gen (HExp text filling)
genHExp = arbitrary >>= genHExpNat 

instance (Arbitrary text, Arbitrary filling) => Arbitrary (HExp text filling) where
    arbitrary :: Gen (HExp text filling)
    arbitrary = genHExp
