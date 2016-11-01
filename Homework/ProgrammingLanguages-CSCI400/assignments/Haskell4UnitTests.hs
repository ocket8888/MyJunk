module Main where

import ChapterFour
import Control.Exception
import Data.Int
import Test.HUnit
import Data.List


main :: IO ()
main = do
    testMinimum
    testCalcSum
    testCount    
    testMakeRange
    testMakeReverseRange
    testNotInList        
    testSquareAll
    testSquareIfEven
    testSquareOnlyEven
    testMergeSort
    testSubList  


testMinimum = Control.Exception.catch (do
        putStrLn $ "\n------ Testing minimum' ------" 
        assertEqual "Failed minimum" (10) (minimum' [56, 88, 900000, 10, 34])
        assertEqual "Failed minimum negative value" (-7) (minimum' [0, 6, -5, -7, 13, 24]) 
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testCalcSum = Control.Exception.catch (do
        putStrLn $ "\n------ Testing calcSum ------" 
        assertEqual "Failed empy list" (0) (calcSum [])
        assertEqual "Failed positive list" (42) (calcSum [8,2,5,5,7,3,12])
        assertEqual "Failed negative list" (-1) (calcSum [0,5,-6,-7, 5, 2])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testCount = Control.Exception.catch (do
        putStrLn $ "\n------ Testing count ------" 
        assertEqual "Failed empty List" (0) (count [])
        assertEqual "Failed regular count" (5) (count [1,2,3,4,5])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testMakeRange = Control.Exception.catch (do
        putStrLn $ "\n------ Testing makeRange ------" 
        assertEqual "Failed basic range" ([5,6,7,8,9,10]) (makeRange 5 10)
        assertEqual "Failed zero" ([0]) (makeRange 0 0)
        assertEqual "Failed negative to positive" ([-2,-1,0,1,2]) (makeRange (-2) 2)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testMakeReverseRange = Control.Exception.catch (do
        putStrLn $ "\n------ Testing makeReverseRange ------" 
        assertEqual "Failed basic reverse range" ([10,9,8,7,6,5]) (makeReverseRange 5 10)
        assertEqual "Failed zero reverse range" ([0]) (makeReverseRange 0 0)
        assertEqual "Failded positive to negative" ([2,1,0,-1,-2]) (makeReverseRange (-2) 2) 
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testNotInList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing notInList ------" 
        assertEqual "Failed empty list" (True) (notInList 77 [])
        assertEqual "Failed in list" (False) (notInList 1 [4,5,6,7,1,9,0])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testSquareAll = Control.Exception.catch (do
        putStrLn $ "\n------ Testing squareAll ------" 
        assertEqual "Failed empty list" ([]::[Int]) (squareAll [])
        assertEqual "Failed regular list" ([1,4,144]) (squareAll [1,2,12])
        assertEqual "Failed negative list" ([1,4,144]) (squareAll [-1,-2,-12])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testSquareIfEven = Control.Exception.catch (do
        putStrLn $ "\n------ Testing squareIfEven ------" 
        assertEqual "Failed emtpy list" ([]::[Int]) (squareIfEven [])
        assertEqual "Failed regular list" [1,4,3,36] (squareIfEven [1,2,3,6])
        assertEqual "Failed negative list" [-1,-3,16,36] (squareIfEven [-1,-3,-4,-6])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testSquareOnlyEven = Control.Exception.catch (do
        putStrLn $ "\n------ Testing squareOnlyEven ------" 
        assertEqual "Failed empty list" ([]::[Int]) (squareOnlyEven [])
        assertEqual "Failed regular list" ([4,64,144]) (squareOnlyEven [2,8,5,7,12])
        assertEqual "Failed all odds" ([]::[Int]) (squareOnlyEven [1,3,5])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testMergeSort = Control.Exception.catch (do
        putStrLn $ "\n------ Testing mergeSort ------" 
        assertEqual "Failed both empty list" ([]::[Int]) (mergeSort [] [])
        assertEqual "Failed left empty list" ([1,2,3]) (mergeSort [] [1,2,3])
        assertEqual "Failed right empty list" ([1,2,3]) (mergeSort [1,2,3] [])
        assertEqual "Failed to merge short left" ([1,2,3,4,5,6,7]) (mergeSort [1,5,6] [2,3,4,7])
        assertEqual "Failed to merge short right" ([1,2,3,4,5,6,7]) (mergeSort [1,5,6,7] [2,3,4])
        assertEqual "Failed to merge equal size" ([1,2,3,4,5,6]) (mergeSort [1,2,3] [4,5,6])
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testSubList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing subList ------" 
        assertEqual "Failed empty list" ([]::[Int]) (subList 0 4 [])
        assertEqual "Failed regular sublist" [4,5,6] (subList 3 5 [0,1,2,4,5,6])
        assertEqual "Failed out of range" [7,10, 11] (subList 3 100 [0,1,2,7,10,11])
        assertEqual "Failed regular sublist" [3, 4, 5] (subList 2 3  [1,2,3,4,5,6,7] )
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

