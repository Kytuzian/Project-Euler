-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
--
-- For example,
--
-- 44 → 32 → 13 → 10 → 1 → 1
-- 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
--
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
--
-- How many starting numbers below ten million will arrive at 89?

chain :: Int -> Bool
chain 1 = False
chain 89 = True
chain x = chain $ unpackNum $ show x

unpackNum :: String -> Int
unpackNum [] = 0
unpackNum [x] = (read (x:[]))^2
unpackNum (x:xs) = (read (x:[]))^2 + unpackNum xs
--
answer :: Int -> [Int]
answer x = filter chain $ map unpackNum $ map show [1..x]

-- "12" -> [1,2] -> 1^2 + 2^2 = 5 -> "5" -> [5] -> 5^2 = 25 -> "25"

main = print $ length $ answer 10000000
