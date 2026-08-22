{-|
Module      : HExpInternalSpec
Description : Testing spec for the holey expressions API
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Various properties of the internals of the text templates API.
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module  Data.HoleyExp.HExpInternalSpec (spec) where

import Test.Hspec            
import Test.QuickCheck.HExp
import Test.Helpers                       (parseTest)
import Data.HoleyExp.HExpInternal
import Data.HoleyExp.Text
import Test.QuickCheck                     (Property
                                           ,Testable (property)
                                           ,verboseCheck
                                           ,Arbitrary)
import Test.Hspec.QuickCheck               (prop)
import Data.Text                           (Text)

import Test.Helpers                        (UnitTest(..)
                                           ,test_case, testParser)
import Data.Maybe                          (isJust)
import Text.Megaparsec                     (parse)

spec :: Spec 
spec = do
    describe "QuickCheck properties:" $ do        
        describe "composition" $ do
            prop "associativity" $
                prop_associativeCompose
            prop "identity" $
                prop_identityCompose
    describe "Unit Tests:" $ do
        describe "Parsing:" $ do
            describe "Holes:" $ do
                test_case "no index"                 test_parseFail1
                test_case "negative index"           test_parseFail2
                test_case "no opening brace"         test_parseFail3
                test_case "no closing brace"         test_parseFail4
                test_case "non-escaped curly brace"  test_parseFail5
                test_case "non-escaped backslash"    test_parseFail6
                test_case "filling in unit template" test_parseFail7

prop_associativeCompose 
    :: HExp Text Text
    -> HExp Text Text
    -> HExp Text Text
    -> Property
prop_associativeCompose t1 t2 t3 = property $ 
    t1 +> (t2 +> t3) == (t1 +> t2) +> t3

prop_identityCompose 
    :: HExp Text Text
    -> Property
prop_identityCompose t = property $ 
    (empty +> t) == t && (t +> empty) == t

testParseHExp :: Parser (HExp Text Text)
testParseHExp = templateParser

testParseUnitTemplate :: Parser (HExp Text ())
testParseUnitTemplate = templateParser

test_parseFail1 :: UnitTest (Maybe (HExp Text Text))
test_parseFail1 = UnitTest {
         test_result=parseTest testParseHExp "foo${a}"
        ,test_output=Nothing
    }

test_parseFail2 :: UnitTest (Maybe (HExp Text Text))
test_parseFail2 = UnitTest {
         test_result=parseTest testParseHExp "foo$-1{a}"
        ,test_output=Nothing
    }

test_parseFail3 :: UnitTest (Maybe (HExp Text Text))
test_parseFail3 = UnitTest {
         test_result=parseTest testParseHExp "foo$1a}bar"
        ,test_output=Nothing
    }

test_parseFail4 :: UnitTest (Maybe (HExp Text Text))
test_parseFail4 = UnitTest {
         test_result=parseTest testParseHExp "foo$1{abar"
        ,test_output=Nothing
    }

test_parseFail5 :: UnitTest (Maybe (HExp Text Text))
test_parseFail5 = UnitTest {
         test_result=parseTest testParseHExp "foo$1{{a}bar"
        ,test_output=Nothing
    }

test_parseFail6 :: UnitTest (Maybe (HExp Text Text))
test_parseFail6 = UnitTest {
         test_result=parseTest testParseHExp "foo$1{\\a}bar"
        ,test_output=Nothing
    }

test_parseFail7 :: UnitTest (Maybe (HExp Text ()))
test_parseFail7 = UnitTest {
         test_result=parseTest testParseUnitTemplate "foo$1{aa}bar"
        ,test_output=Nothing
    }
