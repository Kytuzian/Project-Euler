import Lib (digits, zipTo, count, isSquare)

import Data.Set (deleteFindMin, union, fromList)
import Data.List (inits, tails, (\\))
import qualified Data.List.Ordered as Ord


import System.ProgressBar

isPalindromeNumber :: Integral a => a -> Bool
isPalindromeNumber n = nDigits == reverse nDigits
    where nDigits = digits n

sumOfConsecSquares :: Integral a => a -> [a]
sumOfConsecSquares n = isSumOfConsecSquares' 1 1 0
    where isSumOfConsecSquares' s i a
            | i^2 > n = []
            | a + i^2 > n = isSumOfConsecSquares' (s + 1) (s + 1) 0
            | a + i^2 == n = [s..i]
            | otherwise = isSumOfConsecSquares' s (i + 1) (a + i^2)

-- Squares
a000290_list = scanl (+) 0 [1, 3..]

-- Numbers that are the sums of consecutive squares
a034705_list = f 0 (tail $ inits $ a000290_list) (fromList [0]) where
   f x vss'@(vs:vss) s
     | y < x = y : f x vss' s'
     | otherwise = f w vss (union s $ fromList $ scanl1 (+) ws)
     where ws@(w:_) = reverse vs
           (y, s') = deleteFindMin s

main = do
    let limit = 10^3
    let res = filter (not . isSquare) $ filter isPalindromeNumber $ takeWhile (< limit) $ a034705_list
    -- mapM_ (\i -> progressBar (msg $ show i ++ " of " ++ show limit) percentage 80 i limit) res
    mapM_ (\i -> putStrLn $ (show i ++ ": " ++ (show $ fromIntegral i / fromIntegral limit * 100.0) ++ "%")) res
    print res
    print $ sum res
    print $ length res
