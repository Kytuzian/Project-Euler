import Lib (combinationElements)

import Data.List

isInteger :: RealFrac a => a -> Bool
isInteger n = (fromIntegral $ round n) == n

isTargetNum :: RealFrac a => a -> Bool
isTargetNum n = n > 0 && isInteger n

allVals :: Fractional a => [a] -> [a]
allVals ns = map (\(nums, ops) -> genVals nums ops) opNumPermutations
    where opNumPermutations = [(nums, ops) | nums <- permutations ns, ops <- opPermutations]
          opPermutations = combinationElements (replicate (length ns - 1) [(+),(-),(*),(/)])
          genVals (x:xs) ops = foldl (\cur (op, v) -> cur `op` v) x (zip ops xs)

getTargetNums :: (RealFrac a, Integral b) => [a] -> [b]
getTargetNums ns = sort $ map round $ filter isTargetNum $ nub $ map abs $ allVals ns

countStreak :: (Eq a, Num a, Num b) => [a] -> b
countStreak [] = 0
countStreak (_:[]) = 1
countStreak (x1:x2:xs)
    | x1 + 1 == x2 = 1 + countStreak (x2:xs)
    | otherwise = 1

p93Length :: (RealFrac a, Num b) => [a] -> b
p93Length xs = countStreak $ getTargetNums xs

showMaxBy :: (Show a, Ord b) => (a -> b) -> [a] -> IO a
showMaxBy f (v:vs) = do
    print v
    showMaxBy' v vs
    where showMaxBy' cur [] = return cur
          showMaxBy' cur (x:xs)
            | f x > f cur = do
                print x
                showMaxBy' x xs
            | otherwise = showMaxBy' cur xs

main = do
    let allStreaks = [(p93Length [a,b,c,d], map floor [a, b, c, d]) | a <- [1..20], b <- [a + 1..20], c <- [b + 1..20], d <- [c + 1..20]]
    -- mapM_ print allStreaks
    showMaxBy fst allStreaks
    print $ maximum allStreaks
