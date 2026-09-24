{-|
Module      : HExpInternalSpec
Description : Testing spec for the holey expressions API
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

Various properties of the holey-expressions API.
-}
module  Data.HoleyExp.TextSpec (spec) where

import Data.Maybe (fromMaybe) -- Unsure if necessary, maybe this script already imports a helper that does the same thing.
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
        -- test_case "second test case" second_test
        -- test_case "brace test" brace_test
        -- test_case "empty hole" empty_hole_test
        test_case "plug test" plug_test
        test_case "update replaces filling" update_fill
        test_case "update removes filling" remove_fill

testParseHExp :: Parser (HExp Text Text)
testParseHExp = hExpParser

test_example :: UnitTest (Maybe (HExp Text Text))
test_example = UnitTest {
         test_result=parseTest testParseHExp "foo$1(aabar"
        ,test_output=Nothing
    }

{- Dummy test case -}
second_test :: UnitTest Bool
second_test = UnitTest {
         test_output=True
        ,test_result=True
}

{- Another unit test - Check if braces function works -}
brace_test :: UnitTest Text
brace_test = UnitTest {
         test_output="{a}"
        ,test_result=braces "a"
}

-- This test attempts to check a Text value against a HExp
empty_hole_test :: UnitTest (HExp Text Text)
empty_hole_test = UnitTest {
         test_output="Test is $1()"
        ,test_result=(chunk "Test is ") +> (empty 1) :: HExp Text Text
}

plug_test :: UnitTest (HExp Text Text)
plug_test = UnitTest {
         test_output=(chunk "This is a test")
        ,test_result=fromMaybe "" (plug ((chunk "This is a " ) +> (empty 1) :: HExp Text Text) 1 "test")
}

{- Update on Filled Hole -}
update_fill :: UnitTest (Maybe (HExp Text Text))
update_fill = UnitTest {
         test_output=Just ((chunk "Name: ") +> (filled 1 "Hyde"))
        ,test_result=update ((chunk "Name: ") +> (filled 1 "Jekyll")) 1 (Just "Hyde")

}

{- Empty a filled hole -}
remove_fill :: UnitTest (Maybe (HExp Text Text))
remove_fill = UnitTest {
         test_output=Just ((chunk "This ") +> (empty 1) +> (chunk " is missing."))
        ,test_result=update ((chunk "This ") +> (filled 1 "word") +> (chunk" is missing.")) 1 Nothing
}