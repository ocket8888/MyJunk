module ChapterThree where

inList target list = [ x | x <- list, x == target ]

square x = x*x

--Wasn't this in the chapter 1 homework?
squareEvenNumbers xs = [square x | x <- xs, even x]

-- Yes I'm constructing lists for one element just to concatenate it
--to another string, but it still passes the tests and uses an
--as-pattern, so there. Furthermore, the concatenation operator ought
--to be able to implicitly concatenate lists of the same type AND a
--list with an element of the same type as its contents. Of course,
--that would require the ability to overload functions based on call
--signature.
courseMajor s@(one:two:rest) = s ++ " is a " ++ [one] ++ [two] ++ " course"

-- I don't really know what you wanted in the 'where' clause. I'd have
--guessed constants, but that's the focus of the next question sooo...
--Of course, functions are the one after that so ¯\_(ツ)_/¯ I put in
--just something. The whole point was just to know that 'where' exists
--anyway, right?
threshold price qty scale
    | total < scale = "Total is low"
    | total < 2*scale = "Total is medium"
    | total < 3*scale = "Total is high"
    | total >= 3*scale = "Total is extraordinary"
    where total = price*qty

--"Tabs are literally Satan" - ghc.
lactate hr max
    | level < easy = "warmup"
    | level < aerobic = "easy"
    | level < steadyState = "aerobic"
    | level < anaerobic = "steadyState"
    | level < competitive = "anaerobic"
    | otherwise = "wow, don't do this for long!"
    where level = hr / max
          easy = 0.6
          aerobic = 0.7
          steadyState = 0.8
          anaerobic = 0.9
          competitive = 1

calcAreas xs = [rectArea x | x <- xs]
    where rectArea pair = (fst pair)*(snd pair)

-- It took me literally two hours to figure out that the reason this
--function wouldn't compile is for the same reason I was having
--issues with lactate, and that's that Haskell enforces its style
--guide as a part of the language specification, and the word 'where'
--doesn't count as the beginning of an indentation block. Go figure.
--In fairness, that was probably covered in one of the plethora of
--classes I skipped.
calcTriangleAreas xs = [triArea x | x<-xs]
    where triArea:: (Int, Int) -> Double
          triArea (base, height) = ( fromIntegral (base*height))/2.0

--simpler without guards
orderTwo xs
    | (head xs) < (last xs) = xs
    | otherwise = reverse xs

--eh
orderThree xs
    | a < head pair = [a] ++ pair
    | a > last pair = pair ++ [a]
    | otherwise = [(head pair), a, (last pair)]
    where pair = orderTwo (tail xs)
          a = head xs


-- Well firstly, the pattern-matching provided for Haskell is a
-- good way to parse out parts of an iterable primitive. If you
-- want to compare the second element of a list to something in
-- just a pure guard, you'd have to do something like 
-- 			`head (tail xs)`
-- whereas in a pattern you can directly assign this to a
-- function-scoped variable like so:
-- 			`myFunc _:x:xs = ops`
-- So in general, if you need to parse an iterable primitive,
-- it's best to do it using a pattern.

-- A good use-case for guards, on the other hand, is any time
-- you want to capture the value of something to decide what
-- to do with it, as opposed to just the form. For example,
-- in the `lactate` function, we test whether the input falls
-- within certain value ranges. There's simply no way to do
-- that purely with patterns. Any pattern-matching you do
-- would need to be supplemented by if-then-else blocks. So,
-- if you need to use different function bodies based on the
-- values of the inputs, it's much cleaner and more readable
-- to use guards than to use a pattern that may or may not
-- get you anywhere and then throw the expression into a
-- string of conditionals anyway.