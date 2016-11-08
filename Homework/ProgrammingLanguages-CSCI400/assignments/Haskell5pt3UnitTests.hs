module Main where

import ChapterFive_Pt3
import Control.Exception
import Data.Int
import Test.HUnit
import Data.List


main :: IO ()
main = do
    showRequiredComment
    testGetUserName
    testNumAs
    testTotalDiscount
    testTotalWithDiscount
    testDiscountedItems
    testAnyBigNumbers
    testMultTableRow
    testTestInverses
    
-- assertEqual "" value ttest

showRequiredComment :: IO ()
showRequiredComment = putStrLn $ "\n------ REVIEW question 7, explanation of AND (2 points) ------" 

testGetUserName = Control.Exception.catch (do
        putStrLn $ "\n------ Testing getUserName ------" 
        assertEqual "Failed getUserName" "ldunekac" (getUserName "ldunekac@mines.edu")
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testNumAs = Control.Exception.catch (do
        putStrLn $ "\n------ Testing numAs ------" 
        assertEqual "Failed numAs" 3 (numAs [88,90,91,92,89,87])
        assertEqual "Failed numAs" 0 (numAs [88,62, 75])
        assertEqual "Failed numAs" 0 (numAs [])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

                
testTotalDiscount = Control.Exception.catch (do
        putStrLn $ "\n------ Testing totalDiscount ------" 
        assertEqual "Failed totalDiscount" 2.0 (totalDiscount 0.1 [4,6,10])
        putStrLn $ "   Passed all tests - BE SURE fold used!"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testTotalWithDiscount = Control.Exception.catch (do
        putStrLn $ "\n------ Testing totalWithDiscount ------" 
        assertEqual "Failed totalWithDiscount" 18.0 (totalWithDiscount 0.1 [4, 6, 10])
        putStrLn $ "   Passed all tests - BE SURE fold used!"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testDiscountedItems = Control.Exception.catch (do
        putStrLn $ "\n------ Testing discountedItems ------" 
        assertEqual "Failed discountedItems" [3.6,4.5,5.4,6.3] (discountedItems 0.1 [4,5,6,7])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testAnyBigNumbers  = Control.Exception.catch (do
        putStrLn $ "\n------ Testing anyBigNumbers ------" 
        assertEqual "Failed anyBigNumbers" True (anyBigNumbers 51 [78,2,1,67,98,2,100,2,3,4,2,89])
        assertEqual "Failed anyBigNumbers" False (anyBigNumbers 51 [50, 51])
        assertEqual "Failed anyBigNumbers" True (anyBigNumbers 51 [4, 52])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testMultTableRow   = Control.Exception.catch (do
        putStrLn $ "\n------ Testing multTableRow  ------" 
        assertEqual "Failed multTableRow " [71,142,213,284,355,426,497,568,639,710] (multTableRow 71)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testTestInverses  = Control.Exception.catch (do
        putStrLn $ "\n------ Testing testInverses  ------" 
        assertEqual "FailedInverse f1 f2" False (testInverses f1 f2)
        assertEqual "FailedInverse f2 f3" False (testInverses f2 f3)
        assertEqual "FailedInverse f3 f1" True (testInverses f3 f1)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

