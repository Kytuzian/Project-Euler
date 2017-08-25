import Library.General (memoize)
import Library.Math (toDigits)

squareDigitChain :: Int -> Int
squareDigitChain 1 = 1
squareDigitChain 89 = 89
squareDigitChain n = squareDigitChain $ sum $ map (^2) $ toDigits n

squareDigitChainList :: Integral a => a -> [a]
squareDigitChainList 1 = [1]
squareDigitChainList 89 = [89]
squareDigitChainList n = n : (squareDigitChainList $ sum $ map (^2) $ toDigits n)

memSquareDigitChain = memoize squareDigitChain

doP92 limit = length $ filter (== 89) $ map squareDigitChain [1..limit]

main = print $ doP92 10000000
