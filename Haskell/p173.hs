-- import Lib (intSqrt, collectWhile, zipTo)

squareLaminaeArea :: (Int, Int) -> Int
squareLaminaeArea (s1, s2) = s1^2 - s2^2

squareLaminae :: Int -> [(Int, Int)]
squareLaminae s = [(s, s2) | s2 <- [s - 2, s - 4..start]]
    where start = 2 - s `mod` 2

squareLaminaeWithArea :: Int -> [(Int, Int)]
squareLaminaeWithArea a = concat $ map (takeWhile ((<= a) . squareLaminaeArea)) $ map squareLaminae [1..a]

main = do
    let goal = 10^6
    let sLs = squareLaminaeWithArea goal
    -- mapM_ print $ zip [1..] $ sLs
    print $ length sLs
