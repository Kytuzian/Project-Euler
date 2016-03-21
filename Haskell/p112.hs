import Data.Ratio

import Lib (count, digits, testPair, binarySearch)

isIncreasing :: Integral a => a -> Bool
isIncreasing n = all (testPair (>=)) $ zip nDigits (tail nDigits)
    where nDigits = digits n

isDecreasing :: Integral a => a -> Bool
isDecreasing n = all (testPair (<=)) $ zip nDigits (tail nDigits)
    where nDigits = digits n

isBouncy :: Integral a => a -> Bool
isBouncy n = not (isIncreasing n) && not (isDecreasing n)

bouncyNS :: Integral a => a -> Int
bouncyNS limit = count isBouncy [1..limit]

p112 = do
    let searchSpace = map (\i -> (bouncyNS i, i)) [1..2*10^6]
    res <- binarySearch (\(res, i) -> compare (99 % 100) (res % i)) searchSpace
    return res

main = do
    res <- p112
    case res of
        Just a -> print a
        Nothing -> print "Not found"
