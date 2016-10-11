module Main where

import ChapterOne
import Control.Exception
import Data.Int
import Test.HUnit
import Data.List

main :: IO ()
main = do
    testNegsInList
    testOddsInList
    testCapsInList
    testSumOdds
    testGetQuantities
    testGetPrices
    testGetTotal
    testDoubleAll
    testDoubleFirstList
    testDoubleSecondList
    testSumListItems


testNegsInList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing negsInList ------"
        assertEqual "failed functionally" (3) (negsInList [1,-2,-3,4,5,-10, 0])
        assertEqual "failed functionally" (0) (negsInList [1,2,3,4,5,10, 0]) 
        assertEqual "Empty List failed"   (0) (negsInList []) 
        putStrLn $ "   Passed all inList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testOddsInList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing oddsInList ------"
        assertEqual "failed functionally" 0 (oddsInList [2,4,10,112]) 
        assertEqual "failed functionally" 2 (oddsInList [3,4,10,111]) 
        assertEqual "failed empy list"    0 (oddsInList []) 
        putStrLn $ "   Passed all inList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testCapsInList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing capsInList ------"
        assertEqual "failed functionally"  2  (capsInList ['a', 'b', 'C', 'D', 'e'])
        assertEqual "failed functionally"  0  (capsInList ['a', 'b', 'z'])
        assertEqual "failed empy list"     0  (capsInList [])
        putStrLn $ "   Passed all inList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testSumOdds = Control.Exception.catch (do
        putStrLn $ "\n------ Testing sumOdds ------"
        assertEqual "failed functionally" 15 (sumOdd [1,2,3,4,11])
        assertEqual "failed functionally" 0  (sumOdd [10,2,30,4,110]) 
        assertEqual "failed empy list"    0  (sumOdd [])
        putStrLn $ "   Passed all sumOdds tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex



gradingReceipt = [(1,2),(4,5),(6,7)]


testGetQuantities = Control.Exception.catch (do
        putStrLn $ "\n------ Testing getQuantities ------"
        assertEqual "failed functionally" [1,4,6] (getQuantities gradingReceipt) 
        assertEqual "failed empy list" [] (getQuantities []::[(Int,Int)]) 
        putStrLn $ "   Passed all getQuantities tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testGetPrices = Control.Exception.catch (do
        putStrLn $ "\n------ Testing getPrices ------"
        assertEqual "failed functionally" [2,5,7] (getPrices gradingReceipt) 
        assertEqual "failed empy list" [] (getPrices []::[(Int,Int)])
        putStrLn $ "   Passed all getPrices tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testGetTotal = Control.Exception.catch (do
        putStrLn $ "\n------ Testing getTotal ------"
        assertEqual "failed functionally" 64 (getTotal gradingReceipt)
        assertEqual "failed empty list" 0 (getTotal []) 
        putStrLn $ "   Passed all getTotal tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


gradingList = [[-5,-6,7],[4,7,-9],[1,0,7]] :: [[Int]]

testDoubleAll = Control.Exception.catch (do
        putStrLn $ "\n------ Testing doubleAll ------"
        assertEqual "failed functionally" [[-10,-12,14],[8,14,-18],[2,0,14]] (doubleAll gradingList) 
        assertEqual "failed empty list" [] (doubleAll [])
        putStrLn $ "   Passed all doubleAll tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testDoubleFirstList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing doubleFirstList ------"
        assertEqual "failed functionally" [-10,-12,14] (doubleFirstList gradingList) 
        assertEqual "failed empty list" [] (doubleFirstList [[],[],[]])
        putStrLn $ "   Passed all doubleFirstList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testDoubleSecondList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing doubleSecondList ------"
        assertEqual "failed functionally" [8,14,-18] (doubleSecondList gradingList) 
        assertEqual "failed empty list" [] (doubleSecondList [[],[],[]]) 
        putStrLn $ "   Passed all doubleSecondList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testSumListItems = Control.Exception.catch (do
        putStrLn $ "\n------ Testing sumListItems ------"
        assertEqual "failed functionally" [-4,2,8] (sumListItems gradingList)
        assertEqual "failed empty list" [0,0,0] (sumListItems [[],[],[]]) 
        putStrLn $ "   Passed all sumList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

