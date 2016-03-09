import Lib (memoize)

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = n `mod` 10 : digits (n `div` 10)

squareDigitChain :: Int -> Int
squareDigitChain 1 = 1
squareDigitChain 89 = 89
squareDigitChain n = memSquareDigitChain $ sum $ map (^2) $ digits n

memSquareDigitChain = memoize squareDigitChain

doP92 limit = length $ filter (== 89) $ map memSquareDigitChain [1..limit]

main = print $ doP92 10000000
