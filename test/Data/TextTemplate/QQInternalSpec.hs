{-# LANGUAGE QuasiQuotes #-}
module Data.TextTemplate.QQInternalSpec (spec) where

import Test.Hspec
import Data.TextTemplate.TemplateInternal
import Data.TextTemplate.TextTemplateQQ

import Data.IntMap qualified as M

spec :: Spec 
spec = do
    describe "quasi-quoting tests:" $ do
        it "chunk test 1" $ do
            (fst testChunk1) `shouldBe` (snd testChunk1)
        it "chunk test 2" $ do
            (fst testChunk2) `shouldBe` (snd testChunk2)
        it "chunk test 3" $ do
            (fst testChunk3) `shouldBe` (snd testChunk3)
        it "hole test 1" $ do
            (fst testHole1) `shouldBe` (snd testHole1)
        it "hole test 2" $ do
            (fst testHole2) `shouldBe` (snd testHole2)
        it "hole test 3" $ do
            (fst testHole3) `shouldBe` (snd testHole3)
        it "hole test 4" $ do
            (fst testHole4) `shouldBe` (snd testHole4)
        it "hole test 5" $ do
            (fst testHole5) `shouldBe` (snd testHole5)          
        it "filled hole test 1" $ 
            (fst testFilledHole1) `shouldBe` (snd testFilledHole1)

testChunk1 :: (Template Text (),Template Text ())
testChunk1 = ([unitTemplateQQ|this is a chunk|],chunk "this is a chunk")

testChunk2 :: (Template Text (),Template Text ())
testChunk2 = ([unitTemplateQQ| |],Template (IChunk " ") emptyHoleProps)

testChunk3 :: (Template Text (),Template Text ())
testChunk3 = ([unitTemplateQQ|😩|],Template (IChunk "😩") emptyHoleProps)

testHole1 :: (Template Text (),Template Text ())
testHole1 = ([unitTemplateQQ|$1{}$2{}$3{}|],Template (ICompose "" 1 (ICompose "" 2 (ICompose "" 3 (IChunk "")))) ([1,2,3],M.empty))

testHole2 :: (Template Text (),Template Text ())
testHole2 = ([unitTemplateQQ|$1{}$2{}-$3{}|],Template (ICompose "" 1 (ICompose "" 2 (ICompose "-" 3 (IChunk "")))) ([1,2,3],M.empty))

testHole3 :: (Template Text (),Template Text ())
testHole3 = ([unitTemplateQQ|this $1{} and $2{} is $1{}|],Template (ICompose "this " 1 (ICompose " and " 2 (ICompose " is " 1 (IChunk "")))) ([1,2],M.empty))

testHole4 :: (Template Text (),Template Text ())
testHole4 = ([unitTemplateQQ|Hi $1{}!|], Template (ICompose "Hi " 1 (IChunk "!")) ([1],M.empty))

testHole5 :: (Template Text (),Template Text ())
testHole5 = ([unitTemplateQQ|Hi ❤️, $1{} ‼|], Template (ICompose "Hi ❤️, " 1 (IChunk " ‼")) ([1],M.empty))

testFilledHole1 :: (Template Text Text,Template Text Text)
testFilledHole1 = ([textTemplateQQ|before-$1{example filling}-and-after|], Template (ICompose "before-" 1 (IChunk "-and-after")) ([],M.fromList [(1,"example filling")]))
