module ChapterOne where

negsInList:: [Int] -> Int
oddsInList:: [Int] -> Int
capsInList:: [Char] -> Int
sumOdd:: [Int] -> Int
--getQuantities:: [(Int,Int)] -> [Int]
--getPrices:: [(Int,Int)] -> [Int]
getTotal:: [(Int,Int)] -> Int
doubleAll:: [[Int]] -> [[Int]]
doubleFirstList:: [[Int]] -> [Int]
doubleSecondList:: [[Int]] -> [Int]
sumListItems:: [[Int]] -> [Int]


negsInList (xs) = sum [ 1 | x <- xs, x < 0 ]

oddsInList (xs) = sum [ 1 | x <- xs, odd x ]

capsInList (xs) = sum [ 1 | x <- xs, (x `elem` ['A'..'Z']) ]

sumOdd (xs) = sum [ x | x <- xs, odd x ]

getQuantities (xs) = [fst x | x <- xs]

getPrices (xs) = [snd x | x <- xs]

getTotal (xs) = sum ([fst x * snd x | x <- xs])

doubleAll (xs) = [[2*x | x <- y] | y <- xs]

doubleFirstList (xs) = [2*x | x <- (head xs)]

doubleSecondList (xs) = [2*x | x <- head(tail xs)]

sumListItems (xs) = [sum(x) | x <- xs]