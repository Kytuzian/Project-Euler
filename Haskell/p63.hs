powers n = map (n^) [1..]

digitCount :: (Show a, Num a) => a -> Int
digitCount = length . show

powerDigitCount n = length $ filter (\i -> i == digitCount (n^i)) ds
    where ds = takeWhile (\i -> i <= digitCount (n^i)) [1..]

main = print $ sum $ map powerDigitCount [1..9]
