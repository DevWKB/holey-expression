{-|
Module      : Helpers
Description : Useful helpers for unit testing
Copyright   : (c) Harley Eades, 2026
              (c) WꓘB, 2026
Maintainer  : harley.eades@pm.me

-}
module Test.Helpers 
    (UnitTest(..)
    ,test_case
    ,testParser                
    ,testParseFile) where

import Test.Hspec
import Text.Megaparsec   (ParsecT
                         ,ParseErrorBundle)
import Data.Maybe        (isJust)
import Data.Either.Extra (eitherToMaybe)

-- | The type of a unit test corresponds to a pair of an output value and an
-- expected result.
data UnitTest a = UnitTest {
     test_output :: a -- ^ Output of a computation
    ,test_result :: a -- ^ Expected result of the test
}

testParser :: (ParsecT e t m a -> t -> Either (ParseErrorBundle t e) a)
           -> ParsecT e t m a 
           -> t 
           -> Maybe a
testParser runParser p = eitherToMaybe . runParser p

-- | Simply, did it parse?
testParseFile :: (ParsecT e t m a -> t -> Either (ParseErrorBundle t e) a) 
                -> ParsecT e t m a 
                -> t 
                -> UnitTest Bool
testParseFile runParser p t = UnitTest {
         test_output = isJust $ testParser runParser p t
        ,test_result = True
    } 

test_case :: (Show a, Eq a) 
          => String 
          -> UnitTest a 
          -> SpecWith ()
test_case label t = it label $ (test_output t) `shouldBe` (test_result t)