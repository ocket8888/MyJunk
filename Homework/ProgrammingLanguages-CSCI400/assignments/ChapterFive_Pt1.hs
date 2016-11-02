module ChapterFive_Pt1 where

    --The vast majority of this is just misuse of '*'...
    --For example...

    convert factor input = input * factor

    --This is just the exact same as
  --doMetersToFeet = * 3.28084
    doMetersToFeet = convert 3.28084

    doMilesToKM = convert 1.60934

    calcSalesTax rate total = total * rate

    doGolden = calcSalesTax 0.03

    doBoulder = calcSalesTax 0.0341

    swap (x,y) = (y,x)

    swapAll xs = map swap xs

    applyIfTrue f x b
        | b = f x
        | otherwise = 0

    calcArea l w = l*w

    calcAreas ls ws = zipWith (calcArea) ls ws