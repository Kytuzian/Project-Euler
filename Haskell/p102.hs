import Lib (count, makePairs)

import Data.List.Split

data Triangle = Triangle (Int, Int) (Int, Int) (Int, Int)
    deriving (Show)

crossProduct :: (Int, Int) -> (Int, Int) -> Int
crossProduct (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

dotProduct :: Num a => [a] -> [a] -> a
dotProduct a b = sum $ zipWith (*) a b

sameSide :: (Int, Int) ->  (Int, Int) ->  (Int, Int) ->  (Int, Int) -> Bool
sameSide (p1x,p1y) (p2x,p2y) (ax,ay) (bx,by) = dotProduct [cp1] [cp2] >= 0
    where cp1 = crossProduct ((bx - ax, by - ay)) ((p1x - ax, p1y - ay))
          cp2 = crossProduct ((bx - ax, by - ay)) ((p2x - ax, p2y - ay))

pointIsInTriangle :: (Int, Int) -> Triangle -> Bool
pointIsInTriangle p (Triangle a b c) = sameSide p a b c && sameSide p b a c && sameSide p c a b

makeTriangle :: [(Int, Int)] -> Triangle
makeTriangle (a:b:c:_) = Triangle a b c

readTriangles :: FilePath -> IO [Triangle]
readTriangles path = do
    contents <- readFile path
    let triangleNumbers = map (splitOn ",") $ lines contents
    let triangles = map makePairs $ map (map read) triangleNumbers
    return $ map makeTriangle $ triangles

p102 :: FilePath -> IO Int
p102 path = do
    triangles <- readTriangles path
    return $ count (pointIsInTriangle (0, 0)) triangles

main = do
    res <- p102 "p102_triangles.txt"
    print res
