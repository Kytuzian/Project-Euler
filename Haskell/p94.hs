type Point = (Int, Int)

isIntegral :: RealFrac a => a -> Bool
isIntegral a = (fromIntegral $ round a) == a

triArea :: Floating a => a -> a -> a -> a
triArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

isAlmostEquilateral :: (Floating a, RealFrac a) => a -> a -> a -> Bool
isAlmostEquilateral a b c = isIntegral $ triArea a b c

showSum :: (Show a, Num a) => [a] -> IO a
showSum vals = showSum' vals 0
    where showSum' [] cur = return cur
          showSum' (x:xs) cur = do
            let next = x + cur
            putStrLn $ show x ++ ": " ++ show next
            showSum' xs next

main = do
    let limit = 10^9
    res <- showSum [round (2 * a + c) | c <- [2..limit], let a = c - 1,
                            2 * a + c < limit,
                            isAlmostEquilateral a a c]
    -- mapM_ print tris
    -- let res = zip tris $ scanl (+) 0 tris
    -- mapM_ print res
    print res
