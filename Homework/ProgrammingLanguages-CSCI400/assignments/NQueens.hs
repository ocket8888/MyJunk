import Control.Monad
import Data.List

printBoard board = do
    mapM_ (putStrLn) [[ if entry == True then 'Q' else 'X' | entry <- row] | row <- board]
    putStrLn ""

validateRow board = not (any (\row -> length (filter (==True) row) > 1) board)

validateCol board = validateRow (map reverse (transpose board))

validateDiag board = validateRow (allDiagonals board)

-- ctc Tobias Weck, url: http://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat []) ([]:(diagonals (map tail xss)))

allDiagonals board = (diagonals board) ++ (diagonals (map reverse (transpose board)))

isSln board = (validateRow board) && (validateCol board) && (validateDiag board)

possibleRows length = [(replicate (num - 1) False) ++ (True:(replicate (length - num) False)) | num <- [1..length]]

allBoards size = sequence (replicate size (possibleRows size))

nQueens = do
    putStrLn "Enter Board Size:"
    n <- readLn :: IO Int
    let solution = (filter (isSln) (allBoards n))
    mapM_ (printBoard) solution
    unless ((length solution) > 0) (putStrLn "No solutions found.")

nQueensNumSoln = do
    putStrLn "Enter Board Size:"
    n <- readLn :: IO Int
    print (length (filter (isSln) (allBoards n)))

main = do 
    nQueens
    nQueensNumSoln