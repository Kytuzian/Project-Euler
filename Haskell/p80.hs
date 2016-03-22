import Lib (digits, fromDigits, groupFromStart, intSqrt, isSquare, approximateSquareRoot)

import Data.List

main = do
    let irrational = filter (isSquare)
    let squareRoots = map (sum . take 100 . approximateSquareRoot) $ filter (not . isSquare) [1..100]
    mapM_ print squareRoots
    print $ sum squareRoots
