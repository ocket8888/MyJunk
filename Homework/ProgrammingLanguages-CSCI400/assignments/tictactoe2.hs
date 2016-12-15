{-
The MIT License (MIT)

Copyright (c) 2013 Toby Smith <toby@tismith.id.au>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

--Haskell solution to Tony Morris's TicTacToe challenge
--http://blog.tmorris.net/scala-exercise-with-types-and-abstraction/

module Main where 
import Control.Exception

data Position = TL | TM | TR | ML | MM | MR | BL | BM | BR 
    deriving (Eq, Show, Read, Bounded, Enum)
data Player = One | Two 
    deriving (Eq, Show, Bounded, Enum)
type Move = (Player, Position)
data Unfinished = Unfinished [Move] 
data Finished = Finished [Move]

-- this Either is so that 'move' can
-- return a potentially finished game
type Game = Either Unfinished Finished

class TicTacToe a where
    playerAt :: a -> Position -> Maybe Player
instance TicTacToe Unfinished where
    playerAt (Unfinished moves) = findPlayer moves 
instance TicTacToe Finished where
    playerAt (Finished moves) = findPlayer moves

instance Show Finished where
    show (Finished moves) = showBoard moves
instance Show Unfinished where
    show (Unfinished moves) = showBoard moves

showBoard :: [Move] -> String
showBoard moves = 
        "+---+---+---+\n" ++
        "| " ++ showCell TL moves ++ " | " ++ showCell TM moves ++ " | " ++ showCell TR moves ++ " |\n" ++
        "+---+---+---+\n" ++
        "| " ++ showCell ML moves ++ " | " ++ showCell MM moves ++ " | " ++ showCell MR moves ++ " |\n" ++
        "+---+---+---+\n" ++
        "| " ++ showCell BL moves ++ " | " ++ showCell BM moves ++ " | " ++ showCell BR moves ++ " |\n" ++
        "+---+---+---+\n"

showCell :: Position -> [Move] -> String
showCell p ms = case (findPlayer ms p) of 
        Nothing -> " "
        Just One -> "X"
        Just Two -> "O"

main :: IO ()
main = do
    game <- playGame newGame 
    putStrLn $ "Winner is: " ++ show (whoWon game)
    print game

playGame :: Unfinished -> IO Finished
playGame game = do 
    print game
    position <- getPosition
    let nextGame = move game position
    case nextGame of
        Just goodGame ->
            case goodGame of
                Left unfinishedGame ->
                    playGame unfinishedGame
                Right finishedGame ->
                    return finishedGame
        Nothing ->
	    do 
            invalidMove 
	    playGame game

invalidMove :: IO ()
invalidMove = putStrLn "Invalid move."

printValidMoves :: IO ()
printValidMoves = do
    putStr "Valid moves: "
    print validMoves

getPosition :: IO Position
getPosition = do 
    putStrLn "Please enter move:"
    rawPos <- try getLine
    case rawPos of
        Left (SomeException e) -> 
            do 
            putStrLn "Error. "
            getPosition
        Right rawPosStr ->
            case (reads rawPosStr) of
<<<<<<< HEAD
		[] -> 
		    do
=======
                [] -> 
                    do
>>>>>>> 4d0e588e1269c6c6dcfbf4a9b0db8dc12c2f9022
                    putStrLn "Invalid position."
                    printValidMoves
                    getPosition
                [(goodPos,_)] ->
                    return goodPos

winningPatterns :: [[Position]]
winningPatterns = [[TL,TM, TR], [ML, MM, MR], [BL, BM, BR], 
    [TL, ML, BL], [TM, MM, BM], [TR, MR, BR], [TL, MM, BR],
    [TR, MM, BL]]
validMoves :: [Position]
validMoves = [minBound .. maxBound]

newGame :: Unfinished
newGame = Unfinished []

--this was failing before I introduced the Either to Game
--test = whoWon newGame

-- applies a move to a game
-- must only accept unfinished games
move :: Unfinished -> Position -> Maybe Game
move (Unfinished []) position = Just $ Left (Unfinished [(One, position)])
move game@(Unfinished moves) position
    | not isValid = Nothing
    | isFinished (Unfinished moveList) = Just $ Right (Finished moveList)
    | otherwise = Just $ Left (Unfinished moveList) 
    where
        isValid = validMove game position
        player = whoseTurn game
        moveList = (player, position):moves

validMove :: Unfinished -> Position -> Bool
validMove (Unfinished moves) position = all ((/= position) . snd) moves

isFinished :: Unfinished -> Bool
isFinished (Unfinished []) = False
isFinished (Unfinished moves@((player,_):_)) = 
    any null (foldl removeMove winningPatterns lastPlayersMoves)
    where 
        lastPlayersMoves = map snd $ filter ((== player) . fst) moves
        removeMove winningMoves move = map (filter (/= move)) winningMoves

findPlayer :: [Move] -> Position -> Maybe Player
findPlayer moves position 
    | null move = Nothing
    | otherwise = Just $ (fst . head) move 
    where 
        move = filter ((== position) . snd) moves

-- this should just work on unfinished games
whoseTurn :: Unfinished -> Player
whoseTurn (Unfinished []) = One
whoseTurn (Unfinished ((player,_):_)) = otherPlayer player

otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

-- this should only work on finished games
whoWon :: Finished -> Player
whoWon (Finished ((winner,_):_)) = winner