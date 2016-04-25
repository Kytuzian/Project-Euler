import Lib (isSquare)

import Data.List

main = do
    -- Where a = x + y, b = x + z, c = y + z
    -- and d = x - y, e = x - z, f = y - z
    let f0 = [(a^2, b^2, c^2) | a <- [1..], b <- [1..a - 1], c <- [1..b - 1]]
    let f1 = [(a, b, c, d) | (a, b, c) <- f0, let d = b - c, a > d]
    let f2 = [(a, b, c, d, e) | (a, b, c, d) <- f1, let e = a - c, b > e]
    let f3 = [(a, b, c, d, e, f) | (a, b, c, d, e) <- f2, let f = a - b, c > f]
    let f4 = [(x, a, b, c, d, e, f) | (a, b, c, d, e, f) <- f3,
              let x = (a + d) `div` 2, (a + d) `mod` 2 == 0,
              isSquare d]
    let f5 = [(x, y, a, b, c, d, e, f) | (x, a, b, c, d, e, f) <- f4,
              let y = (a - d) `div` 2, x > y, (a - d) `mod` 2 == 0,
              isSquare f]
    let f6 = [(x, y, z) | (x, y, a, b, c, d, e, f) <- f5,
              let z = (b - e) `div` 2, y > z, (b - e) `mod` 2 == 0,
              isSquare e,
              x > y, y > z]
    print $ head f6
    -- print "----------------------------"
    -- print "f0"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f0
    -- print "----------------------------"
    -- print "f1"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f1
    -- print "----------------------------"
    -- print "f2"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f2
    -- print "----------------------------"
    -- print "f3"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f3
    -- print "----------------------------"
    -- print "f4"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f4
    -- print "----------------------------"
    -- print "f5"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f5
    -- print "----------------------------"
    -- print "f6"
    -- print "----------------------------"
    -- mapM_ print $ zip [1..] $ take 1000 f6
