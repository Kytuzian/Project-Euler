import Data.List

import System.IO

type Point = (Int, Int)

isOriginRightTriangle :: Point -> Point -> Bool
isOriginRightTriangle (x1, y1) (x2, y2) = t + u == v
    -- These are all squared side lengths to save time
    where a = x1^2 + y1^2
          b = x2^2 + y2^2
          c = (x1 - x2)^2 + (y1 - y2)^2
          [t,u,v] = sort [a,b,c]

main = do
    hSetBuffering stdout NoBuffering

    let limit = 50
    let tris = nub $ map sort $ [[(x1, y1), (x2, y2)] | x1 <- [0..limit], y1 <- [0..limit], x2 <- [0..limit], y2 <- [0..limit],
                                            (x1, y1) /= (x2, y2), (x1, y1) /= (0,0), (x2, y2) /= (0,0),
                                            isOriginRightTriangle (x1, y1) (x2, y2)]
    mapM_ (\i -> putStr $ "\r" ++ show i) tris
    putStrLn ""
    print $ length tris
