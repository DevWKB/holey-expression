{-|
Module      : TemplateInternalSpec
Description : Testing spec for the text template API
Copyright   : (c) Harley Eades, 2026
              (c) WꓘB, 2026
Maintainer  : harley.eades@pm.me

Various properties of the internals of the text templates API.
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module  Data.TextTemplate.TemplateInternalSpec (spec) where

import Test.Hspec            
import Test.QuickCheck                     (Property
                                           ,Testable (property)
                                           ,verboseCheck
                                           ,Arbitrary)
import Test.Hspec.QuickCheck               (prop)
import Data.Text                           (Text)

import Test.Helpers                        (UnitTest(..)
                                           ,test_case)
import Data.TextTemplate.TemplateInternal
import Test.QuickCheck.TextTemplate

spec :: Spec 
spec = do
    describe "QuickCheck properties:" $ do        
        describe "composition" $ do
            prop "associativity" $
                prop_associativeCompose
            prop "identity" $
                prop_identityCompose

prop_associativeCompose 
    :: Template Text 
    -> Template Text 
    -> Template Text 
    -> Property
prop_associativeCompose t1 t2 t3 = property $ 
    t1 +> (t2 +> t3) == (t1 +> t2) +> t3

prop_identityCompose 
    :: Template Text 
    -> Property
prop_identityCompose t = property $ 
    (empty +> t) == t && (t +> empty) == t

