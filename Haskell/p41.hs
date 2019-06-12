import Data.List

import Library.Math

isPrime :: Integer -> Bool
isPrime n = isPrime' 2
    where isPrime' i
            | i * i > n = True
            | n `mod` i == 0 = False
            | otherwise = isPrime' $ i + 1

pandigitals :: Integer -> [Integer]
pandigitals n = map fromDigits $ permutations [1..n]

main = print $ maximum $ filter isPrime $ concat $ map pandigitals [1..7]

