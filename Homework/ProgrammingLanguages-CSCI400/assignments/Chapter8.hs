import Control.Monad

greeting = do 
    putStrLn "Please enter your name (<Last>, <First>)"
    name <- getLine
    putStrLn ("Hello" ++ (dropWhile (/=' ') name) ++ ",\nDo you have a relative named Joe "++(takeWhile (/=',') name))

reverseMe = do
    input <- getLine
    mapM_ (putStr) (map (\x -> x ++ " ") (reverse (words input)))
    putStrLn ""

--doing input on the same line as output as is shown in the
--homework description requires flushing the stdout buffer,
--which is technically file I/O and therefore (I think)
--outside the scope of this homework. so I'm prompting on a
--different line than that from which I'm reading input.
sayHi = do
    putStrLn "What is your name?"
    name <- getLine
    when (name == "Bilbo") $ do
        putStrLn "So nice to meet you!"
    putStrLn "What's up?"

printAreas xa xb = mapM_ (print) (zipWith (*) xa xb)

--since the behaviour of this function is undefined when
--the argument is not 'u', 'd', 'l' or 'r', I took a liberty
--so it'll print nothing
printDirection c = do
    when (c =='u') $ do
       putStrLn "UP"
    when (c=='l') $ do
       putStrLn "LEFT"
    when (c=='d') $ do
       putStrLn "DOWN"
    when (c=='r') $ do
       putStrLn "RIGHT"

printDirections xs = do
    forM_ xs (\a -> do
       putStr "You should go "
       printDirection a)