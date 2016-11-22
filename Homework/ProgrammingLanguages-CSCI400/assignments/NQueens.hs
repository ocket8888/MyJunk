import Control.Monad
import Data.List

--prints an individual solution
printBoard board = do
    mapM_ (putStrLn) [[ if entry == True then 'Q' else 'X' | entry <- row] | row <- board]
    putStrLn ""


--checks if a board layout is a solution by verifying that no one queen can attack another
isSln board = (validateRow board) && (validateCol board) && (validateDiag board)

--these three functions validate that queens cannot attack each other
--laterally, vertically, or diagonally
validateRow board = not (any (\row -> length (filter (==True) row) > 1) board)
validateCol board = validateRow (map reverse (transpose board))
validateDiag board = validateRow (allDiagonals board)

-- ctc Tobias Weck, url: http://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat []) ([]:(diagonals (map tail xss)))

--gets all of the diagonals of the board
allDiagonals board = (diagonals board) ++ (diagonals (map reverse (transpose board)))

--generates every possible row of a given size (helpful because each row can only contain 1 queen)
possibleRows length = [(replicate (num - 1) False) ++ (True:(replicate (length - num) False)) | num <- [1..length]]

--gets every possible configuration for a given size of n-queens
allBoards size = sequence (replicate size (possibleRows size))

--asks the user for the board size, then attempts to solve n-queens
--on that board, printing the solutions (or "No solutions found." if none)
nQueens = do
    putStrLn "Enter Board Size:"
    n <- readLn :: IO Int
    let solution = (filter (isSln) (allBoards n))
    mapM_ (printBoard) solution
    unless ((length solution) > 0) (putStrLn "No solutions found.")

--outputs only the number of solutions for a user-specified board size
nQueensNumSoln = do
    putStrLn "Enter Board Size:"
    n <- readLn :: IO Int
    print (length (filter (isSln) (allBoards n)))

--tests the two main functions
main = do
    putStrLn "Testing nQueens..." 
    nQueens
    putStrLn "Testing nQueensNumSoln..."
    nQueensNumSoln