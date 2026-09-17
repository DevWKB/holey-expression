{-|
Module      : HExpInternalSpec
Description : Testing spec for the holey expressions API
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Various properties of the holey-expressions API.
-}
module  Data.HoleyExp.TextSpec (spec) where

import Data.HoleyExp.HExpInternal
import Data.HoleyExp.Text
import Test.QuickCheck.HExp                ()

import Test.Hspec            
import Test.Helpers                        (parseTest)
import Test.QuickCheck                     (Property
                                           ,Testable (property))
import Test.Hspec.QuickCheck               (prop)
import Test.Helpers                        (UnitTest(..)
                                           ,test_case)

spec :: Spec 
spec = do
    describe "Unit Tests:" $ do
        test_case "example test case" test_example
        test_case "second test case" second_test -- Add another case here

testParseHExp :: Parser (HExp Text Text)
testParseHExp = hExpParser

test_example :: UnitTest (Maybe (HExp Text Text))
test_example = UnitTest {
         test_result=parseTest testParseHExp "foo$1(aabar"
        ,test_output=Nothing
    }

{- Will define second test case here -}
second_test :: UnitTest Bool
second_test = UnitTest {
         test_output=True
        ,test_result=True
}