import Lib (digits)

import Data.List ((\\), find)

isPandigital :: Integral a => [a] -> Bool
isPandigital xs = length ([1..9]  \\ xs) == 0

endsWithPandigital :: Integral a => a -> Bool
endsWithPandigital n = isPandigital $ take 9 $ digits n

startsWithPandigital :: Integral a => a -> Bool
startsWithPandigital n = isPandigital $ take 9 $ reverse $ digits n

fibonacci = fibonacci' 0 1
    where fibonacci' a b = a : fibonacci' b (a + b)

main = do
    let ns = zip [0..] fibonacci
    let res = find (\(_,v) -> endsWithPandigital v && startsWithPandigital v) ns
    print res
