module Main where

import ChapterThree
import Control.Exception
import Data.Int
import Test.HUnit
import Data.List



main :: IO ()
main = do
    showHeader
    testInList
    testSquare
    testSquareEvenNumbers
    testCourseMajor
    testThreshold
    testLactate
    testCalcAreas
    testCalcTriangleAreas
    testOrderTwo
    testOrderThree
    showRequiredComment

showHeader :: IO ()
showHeader = putStrLn $ "\n------ Ensure EACH function includes a function signature (5 points) ------" 

showRequiredComment :: IO ()
showRequiredComment = putStrLn $ "\n------ Include comment for guards vs pattern matching (4 points) ------" 
  

testInList = Control.Exception.catch (do
        putStrLn $ "\n------ Testing inList ------" 
        assertEqual "Failed EmptyList" [] (inList 0 [])
        assertEqual "Failed none in list" [] (inList 1 [2,3,4,5])
        assertEqual "Failed one in list" [5] (inList 5 [3,5,7,11])
        assertEqual "Failed two in list" [5,5] (inList 5 [2,5,3,5])
        assertEqual "Failed two in small list" [5,5] (inList 5 [5,5])        
        putStrLn $ "   Passed all inList tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testSquare = Control.Exception.catch (do
        putStrLn $ "\n------ Testing Square ------" 
        assertEqual "failed Zero" 0 (square 0)
        assertEqual "failed 5" 25 (square 5)
        assertEqual "failed negative" 81 (square (0 - 9))
        putStrLn $ "   Passed all Square tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testSquareEvenNumbers = Control.Exception.catch (do
        putStrLn $ "\n------ Testing squareEvenNumbers ------" 
        assertEqual "test empty List" [] (squareEvenNumbers [])
        assertEqual "test no evens" [] (squareEvenNumbers [1,9,101])
        assertEqual "test even numbers with negative" [64, 16, 36] (squareEvenNumbers [3,8,4,1,-6])
        putStrLn $ "   Passed all squareEvenNumbers tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testCourseMajor = Control.Exception.catch (do
        putStrLn $ "\n------ Testing courseMajor ------" 
        assertEqual "test CS course" "CSCI306 is a CS course" (courseMajor "CSCI306")
        assertEqual "test EE course" "EEEN450 is a EE course" (courseMajor "EEEN450")
        putStrLn $ "   Passed all courseMajor tests - BE SURE as-pattern used!"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testThreshold = Control.Exception.catch (do
        putStrLn $ "\n------ Testing threshold ------" 
        assertEqual "test low" "Total is low" (threshold 1 5 10)
        assertEqual "test high" "Total is high" (threshold 6 4 10)
        assertEqual "test medium" "Total is medium" (threshold 3 5 10)
        assertEqual "test medium" "Total is extraordinary" (threshold 6 6 10) 
        assertEqual "test low threshold 5" "Total is low" (threshold 1 3 5)         
        putStrLn $ "   Passed all threshold tests - BE SURE where clause used!"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testLactate = Control.Exception.catch (do
        putStrLn $ "\n------ Testing lactate ------" 
        assertEqual "test warmup" "warmup" (lactate 90 170)
        assertEqual "test aerobic" "aerobic" (lactate 120 170)
        assertEqual "test steady state" "steadyState" (lactate 145 170)
        assertEqual "test anaerobic" "anaerobic" (lactate 165 170) 
        assertEqual "test competitive" "wow, don't do this for long!" (lactate 170 170) 
        putStrLn $ "   Passed all lactate tests - BE SURE constants in where clause"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

testCalcAreas = Control.Exception.catch (do
        putStrLn $ "\n------ Testing calcAreas ------" 
        assertEqual "Test empty list" ([]::[Double]) (calcAreas [])
        assertEqual "Test areas" [30, 24] (calcAreas [(10,3), (6,4)])
        putStrLn $ "   Passed all calcAreas tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testCalcTriangleAreas = Control.Exception.catch (do
        putStrLn $ "\n------ Testing calcTrangleAreas ------" 
        assertEqual "test empty list" ([]::[Double]) (calcTriangleAreas [])
        assertEqual "test list" [10,14] (calcTriangleAreas [(4,5), (7,4)])
        putStrLn $ "   Passed all calcTrangleAreas tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testOrderTwo = Control.Exception.catch (do
        putStrLn $ "\n------ Testing orderTwo ------" 
        assertEqual "Test correct order" [2,5] (orderTwo [2,5])
        assertEqual "Test mixed order" [2,3] (orderTwo [3,2])
        assertEqual "Test equal" [2,2] (orderTwo [2,2])
        putStrLn $ "   Passed all orderTwo tests - BE SURE uses guards!"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testOrderThree = Control.Exception.catch (do
        putStrLn $ "\n------ Testing orderThree ------" 
        assertEqual "Test correct order" [2,5,9] (orderThree [2,5,9])
        assertEqual "Test reverse order" [1,2,3] (orderThree [3,2,1])
        assertEqual "Test mixed order" [1,2,3] (orderThree [1,3,2])
        putStrLn $ "   Passed all orderThree tests - BE SURE uses orderTwo"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

