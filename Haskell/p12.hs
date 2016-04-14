import Lib (count, divides, intSqrt, isSquare)

import Data.List (find)

triangleNumbers :: Integral a => [a]
triangleNumbers = scanl (+) 1 [2..]

divisors :: Integral a => a -> a
divisors n
    | isSquare n = 2 * lowCount + 1
    | otherwise = 2 * lowCount
    where lowCount = count (`divides` n) [1..intSqrt n - 1]

main = do
    let res = find ((> 500) . divisors) triangleNumbers
    print res
