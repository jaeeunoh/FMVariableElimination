module Main where

import Test.Hspec
import Types
import Parser
import Lib
import qualified Data.Map as Map
import Data.Ratio

import TestSets
import Util

successful (Right x) = x
successful (Left err) = error $ show err

parse = successful . parseEquation

spec :: Spec
spec = do
    describe "inequality parser" $ do
        --it "should parse very trivial inequalities" $ parse "x > 0" `shouldBe` RawInequality [VarToken 2 'x'] [ConstantToken 0] ">"
        it "should parse trivial inequalities" $ parse "2x > 0" `shouldBe` RawInequality [VarToken 2 'x'] [ConstantToken 0] ">"

    describe "extracting variables" $ do
        it "should flip everything if the sign is positive" $
            extractOne 'x' (ReducibleInequality (Map.fromList [('x', 1), ('y', 4)]) 0 LessThan)
            `shouldBe`
            ReducibleInequality {leftSide = Map.fromList [('y',-4)], constant = 0, inequalityType = GreaterThan}
        it "shouldn't flip anything if the sign is negative" $
            extractOne 'x' (ReducibleInequality (Map.fromList [('x', -1), ('y', 4)]) 0 LessThan)
            `shouldBe`
            ReducibleInequality {leftSide = Map.fromList [('y',4)], constant = 0, inequalityType = LessThan}

        it "should normalize all coefficients properly if the sign is positive" $
            extractOne 'x' (ReducibleInequality (Map.fromList [('x', 2), ('y', 4)]) 0 LessThan)
            `shouldBe`
            ReducibleInequality {leftSide = Map.fromList [('y',-2)], constant = 0, inequalityType = GreaterThan}
        it "should normalize all coefficients properly if the sign is negative" $
            extractOne 'x' (ReducibleInequality (Map.fromList [('x', -2), ('y', 4)]) 0 LessThan)
            `shouldBe`
            ReducibleInequality {leftSide = Map.fromList [('y',2)], constant = 0, inequalityType = LessThan}

        it "should extract with the proper direction of inequality" $
            extractOne 'x' (prs "1x < 10") `shouldBe` ReducibleInequality Map.empty (10) GreaterThan

    describe "extracting from a set of inequalities" $ do
        it "should reduce the variable properly in a simple (2-vars) set" $
            (reduce startSet) `shouldBe` endSet
        it "should reduce the variable properly in a more complex (3-vars) set" $
            (reduce startSet2) `shouldBe` endSet2
        it "should reduce within a slightly skewed set" $
            reduce startSet3 `shouldBe` endSet3
                    

    describe "double reduction" $ do
        it "should reduce properly one step after another" $
            (reduce endSet2) `shouldBe` endEndSet2

    describe "final checks" $ do
        it "should properly verify trivial cases" $ do
            checkFinal [prs "1 > 0"] `shouldBe` True
            checkFinal [prs "0 > 1"] `shouldBe` False

        it "should properly verify trivial cases in sets" $ do
            checkFinal [prs "1 > 0", prs "2 > 0"] `shouldBe` True
            checkFinal [prs "0 < 1", prs "1 > 0"] `shouldBe` True
            checkFinal [prs "1 < 0", prs "1 > 0"] `shouldBe` False
            checkFinal [prs "1 < 0", prs "0 > 1"] `shouldBe` False

        it "should verify open 1-var cases to true" $ do
            checkFinal [prs "1x > 0"] `shouldBe` True
            checkFinal [prs "1x < 0"] `shouldBe` True
            checkFinal [prs "1x > 0", prs "2x > 0"] `shouldBe` True

        it "should verify no-constant 1-var cases" $ do
            checkFinal [prs "1x > 0", prs "1x < 10"] `shouldBe` True
            checkFinal [prs "1x > 0", prs "1x < -5"] `shouldBe` False

        it "should verify complex cases with constants" $ do
            checkFinal [prs "2x > 6", prs "1x < 5"] `shouldBe` True
            checkFinal [prs "1x > 1", prs "-2x > -4", prs "2x > 3"] `shouldBe` True
            checkFinal [prs "3x < -5", prs "2x > 100"] `shouldBe` False

        it "should verify hybrid cases" $ do
            checkFinal [prs "1x > 10", prs "1 > 0"] `shouldBe` True
            checkFinal [prs "1x > 10", prs "2x < 8", prs "3 > 0"] `shouldBe` False
            checkFinal [prs "1x > 2", prs "2x < 8", prs "3 < 0"] `shouldBe` False


    describe "integration" $ do
        it "should properly solve a simple case" $
            check startSet3 `shouldBe` True
        it "should properly solve a simple negative case" $
            check startSet4 `shouldBe` False
        it "should solve a 3-variable case" $
            check startSet2 `shouldBe` True

main = hspec spec

