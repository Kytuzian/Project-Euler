import Lib (digits, zipTo, count, isSquare, nubOnSorted)

import Data.Set (deleteFindMin, union, fromList)
import Data.List (inits, tails, (\\), nub, sort)

import System.ProgressBar

isPalindromeNumber :: Integral a => a -> Bool
isPalindromeNumber n = nDigits == reverse nDigits
    where nDigits = digits n

genConsecSums :: Integer -> [Integer]
genConsecSums n = nubOnSorted $ sort $ genConsecSums' 0 0 0
    where genConsecSums' s i a
            | i^2 > n = []
            | a + i^2 > n = genConsecSums' (s + 1) (s + 1) 0
            | a + i^2 == n = a + i^2 : genConsecSums' (s + 1) (s + 1) 0
            | otherwise = a + i^2 : genConsecSums' s (i + 1) (a + i^2)

main = do
    let limit = 10^8
    let res = filter (not . isSquare) $ filter isPalindromeNumber $ genConsecSums limit
    mapM_ (\i -> putStrLn $ (show i ++ ": " ++ (show $ fromIntegral i / fromIntegral limit * 100.0) ++ "%")) res
    print res
    print $ sum res
    print $ length res
