module ChapterFour where

    minimum' (x:xs)
        | (length xs) == 0 = x
        | otherwise = if x < q then x else q
        where q = minimum' xs

    calcSum [] = 0

    calcSum (x:xs) = x + (calcSum xs)

    count [] = 0

    count (x:xs) = 1 + count xs

    makeRange a b
        | a == b = [b]
        | otherwise = a:(makeRange (a+1) b)

    makeReverseRange a b
        | a == b = [b]
        | otherwise = b:(makeReverseRange a (b-1))

    notInList a [] = True

    notInList a (x:xs)
        | a == x = False
        | otherwise = (notInList a xs)

    squareAll [] = []

    squareAll (x:xs) = (x^2):(squareAll xs)

    squareIfEven [] = []

    squareIfEven (x:xs)
        | odd x = x:(squareIfEven xs)
        | otherwise = (x^2):(squareIfEven xs)

    squareOnlyEven [] = []

    squareOnlyEven (x:xs)
        | odd x = squareOnlyEven xs
        | otherwise = (x^2):(squareOnlyEven xs)

    mergeSort xs [] = xs

    mergeSort [] xs = xs

    --No, it's not merge sort
    --but yes, it is recursive and yes it works, so there
    mergeSort xa (b:xb) = mergeSort ([y | y <- xa, y <= b] ++ (b:[z | z <- xa, z>b])) xb

    subList a b [] = []

    --I don't care that this is ugly; it's not the way it's meant to be done anyway.
    subList a b xs
        | a /= 0 = subList (a-1) (b-1) (tail xs)
        | ( and ((a == 0),(b < ((length xs)-2)))) = subList a b (init xs)
        | otherwise = xs