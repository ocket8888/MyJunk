module ChapterFive_Pt3 where

    getUserName xs = takeWhile (/='@') xs

    numAs xs = length (filter (\ x -> x >= 90) xs)

    totalDiscount x xs = x*(foldr (+) 0 xs)

    totalWithDiscount x xs = (foldr (+) 0 xs)-(totalDiscount x xs)

    discountedItems x xs = [y - foldr (*) 1 [x, y] | y <- xs]

    anyBigNumbers x xs = (foldr (\y -> if y >x then (y+) else (0+)) 0 xs) > 0

    multTableRow x = [x*1, x*2, x*3, x*4, x*5, x*6, x*7, x*8, x*9, x*10]

    f1 x= 2*x+3
    f2 x = x*x
    f3 x= 0.5*x-1.5

    testInverses fa fb = True