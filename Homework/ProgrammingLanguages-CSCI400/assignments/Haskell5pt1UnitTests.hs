module Main where

import ChapterFive_Pt1
import Control.Exception
import Data.Int
import Test.HUnit
import Data.List
import Text.Printf

main :: IO ()
main = do
    testConvert
    testMetersToFeet
    testMilesToKM
    testTax
    testSwap
    testApplyIfTrue
    testCalcAreas

testConvert = Control.Exception.catch (do
        putStrLn $ "\n------ Testing convert ------" 
        assertEqual "Failed to convert" (20) (convert 4 5)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testMetersToFeet = Control.Exception.catch (do
        putStrLn $ "\n------ Testing doMetersToFeet ------" 
        assertEqual "Failed doMetersToFeet" (3.28084) (doMetersToFeet 1.0)
        assertEqual "Failed doMetersToFeet" (9.84252) (doMetersToFeet 3.0)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testMilesToKM = Control.Exception.catch (do
        putStrLn $ "\n------ Testing doMilesToKM ------" 
        assertEqual "Failed doMilesToKM" (1.60934) (doMilesToKM 1.0)
        assertEqual "Failed doMilesToKM" (8.0467) (doMilesToKM 5.0)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testTax = Control.Exception.catch (do
        putStrLn $ "\n------ Testing doGolden ------" 
        assertEqual "Failed Golden" (0.3) (doGolden 10)
--        assertEqual "Failed Boulder" (0.3410) (doBoulder 10)
--       printf "%.4f\n" $ (doBoulder 10)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testSwap = Control.Exception.catch (do
        putStrLn $ "\n------ Testing swap ------" 
        assertEqual "Failed swap" (7,6) (swap (6,7))
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testApplyIfTrue = Control.Exception.catch (do
        putStrLn $ "\n------ Testing applyIfTrue ------" 
        assertEqual "Failed apply if true" (6) (applyIfTrue (*3) 2 True)
        assertEqual "Failed apply if false" (0) (applyIfTrue (*3) 2 False)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testCalcAreas = Control.Exception.catch (do
        putStrLn $ "\n------ Testing calcAreas ------" 
        assertEqual "Failed calcAreas" [6,24,16] (calcAreas [3,6,8] [2,4,2])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

