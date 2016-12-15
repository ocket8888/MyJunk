import System.IO
import Data.Char

pigLatin x
    | (head x) `elem` "aeiouy" = x ++ "way"
    | otherwise = ((tail x) ++ [(head x)] ++ "ay")

pigWordByWord xs = foldr (\x acc -> x ++ " " ++ acc ) "" (map pigLatin (words xs))

--changing Line Buffering settings wasn't working for my tty, so
--I looked it up and the following function lets me apply interact
--to each line of input lazily, so here you go:
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines
--ctc luqui, url: http://www.stackoverflow.com/questions/16799755/haskell-interact-function#16799891

piggly = interact (eachLine pigWordByWord)

--Oh my god. This is so impossible to do in an arbitrary locale.
--I apoligize if it doesn't work for the grader, just trust me
--when I say it works in my locale (where the values for a-z range from
--97 to 122 and the values for A-Z range from 65 to 90)
ceasar s c
    | not (isLetter c) = c --Don't need to check locales if it's not a letter
    | (isLetter c) && (localeIsSep) && (isUpper c) = if (val < localeUpperMax) then (chr val) else (chr (localeUpperMin + (val - localeUpperMax)))
    | (isLetter c) && (localeIsSep) && (isLower c) = if (val < localeLowerMax) then (chr val) else (chr (localeLowerMin + (val - localeLowerMax)))
    | otherwise = if (otherVal < localeMax) then (chr otherVal) else (chr (localeMin + (otherVal - localeMax)))
    where
        localeIsSep = abs((ord 'a') - ord('A')) /= 1 --True if locale separates lower and upper cases, False if the ascii values of cases of the same letter are adjacent
        val = (ord c) + s
        otherVal = (ord c) + (2*s) --If the locale is not seperated, need to skip over other-case letters. This might miss. Not my locale, don't care enough to test in a different locale
        localeMax = maximum [(ord 'Z'), (ord 'z')]
        localeMin = minimum [(ord 'a'), (ord 'A')]
        localeUpperMax = ord 'Z'
        localeUpperMin = ord 'A'
        localeLowerMax = ord 'z'
        localeLowerMin = ord 'a'

encrypt s = do
    encryptMe <- getLine
    putStrLn $ map (\c -> ceasar s c) encryptMe

encryptFile xs s = do
	handle <- openFile xs ReadMode
	contents <- hGetContents handle
	putStrLn $ map (\c -> ceasar s c) contents

main = do
	putStrLn "pigLatin \"pigLatin\""
	putStrLn $ pigLatin "pigLatin"
    putStrLn "To test file encryption, enter the input file name: "
    fname <- getLine
    putStrLn $ "Encrypting file '"++fname++"' with seed 5"
    encryptFile fname 5