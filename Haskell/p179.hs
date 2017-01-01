import Lib (intSqrt)

import qualified Data.Map as Map

import System.ProgressBar

printFilterMap :: (Show b, Show c) => (a -> Bool) -> (a -> b) -> (a -> c) -> [a] -> IO ()
printFilterMap f trueS falseS vs = mapM_ printFilterMap' vs
    where printFilterMap' v = case f v of
                                  True -> putStrLn ("\r" ++ (show $ trueS v))
                                  False -> putStr ("\r" ++ (show $ falseS v))

divides :: Integral a => a -> a -> Bool
a `divides` b = b `mod` a == 0

divisors :: Integral a => a -> [a]
divisors n = filter (`divides` n) [1..(n `div` 2) + 1] ++ [n]

sieve :: Integral a => a -> [a]
sieve limit = sieve' (Map.fromList $ zip [2..limit] (repeat True)) 2
    where sieve' curMap n = case nextN of
                                Nothing -> map fst $ filter snd $ Map.toList curMap
                                Just v -> sieve' nextMap v
            where nextMap = foldl (\cur k -> Map.insert k False cur) curMap [n * 2, n * 3..limit]
                  nextN = nextN' (n + 1)
                    where nextN' i
                            | i < limit = case Map.lookup i nextMap of
                                            Nothing -> nextN' (i + 1)
                                            Just False -> nextN' (i + 1)
                                            Just True -> Just i
                            | otherwise = Nothing

primes = sieve (10^4)

-- factor :: Integer -> [(Integer, Int)]
factor n = factor' n primes
    where factor' 1 _ = []
          factor' cur [] = [(cur, 1)]
          factor' cur (i:is)
            | i * i > n = [(cur, 1)]
            | count > 0 = (i, count) : factor' nextCur is
            | otherwise = factor' nextCur is
            where (nextCur, count) = getDivCount cur 0
                  getDivCount v count
                    | v `mod` i == 0 = getDivCount (v `div` i) (count + 1)
                    | count > 0 = (v, count)
                    | otherwise = (v, 0)

-- d :: Integral a => a -> Int
d n = product $ map ((+ 1) . snd) $ factor n

p179 ns = length $ filter id $ zipWith (==) ns (drop 1 ns)

main = do
    let limit = 10^7
    let divisors = zip [1..] $ map d [1..limit]
    -- mapM_ (\i -> putStr $ "\r" ++ show i) divisors
    -- putStrLn ""

    let res = p179 $ map snd divisors
    print res
