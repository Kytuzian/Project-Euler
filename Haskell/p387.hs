import Lib (sumDigits, divides, truncates, startsWithAny, digits)

import Math.NumberTheory.Primes

isHarshadNumber :: Integral a => a -> Bool
isHarshadNumber n = (sumDigits n) `divides` n

isRightTruncatableHarshadNumber :: Integral a => a -> Bool
isRightTruncatableHarshadNumber n = all isHarshadNumber $ truncates n

isStrongNumber :: Integer -> Bool
isStrongNumber n = isPrime $ n `div` (sumDigits n)

isStrongRightTruncatableHarshadPrime :: Integer -> Bool
isStrongRightTruncatableHarshadPrime n = isHarshadNumber n1 && isStrongNumber n1 && isRightTruncatableHarshadNumber n1
    where n1 = head $ truncates n

main = do
    let limit = 10^14
    let res1 = filter (even . last . digits) $ takeWhile (< limit) $ drop 4 primes
    let res = filter isStrongRightTruncatableHarshadPrime res1
    mapM_ print res
    print $ sum res
