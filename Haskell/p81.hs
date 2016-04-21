import Lib ((!!!), flatten, separateList, minimumIndexBy, inMatrix, sumPair,
            remove, mapPair, setAt2, zipTo, distance)

import Data.List.Split
import Data.Ord
import Data.List

data Direction = L | R | U | D

direction :: Integral a => Direction -> (a, a)
direction L = (-1, 0)
direction R = (1, 0)
direction U = (0, -1)
direction D = (0, 1)

backwards :: Integral a => Direction -> (a, a)
backwards d = mapPair (*(-1)) $ direction d

fill :: (Int, Int) -> a -> [[a]]
fill (w, h) e = replicate h (replicate w e)

minPathSums ds (x, y) matrix = minPathSums' base
    where minPathSums' cur = foldl (\prev coords -> calcNext coords prev) cur nexts
            where nexts = sortBy (comparing (distance (x, y))) allCoords
                  allCoords = [(x, y) | x <- [0..x], y <- [0..y]]
                  neighbors c m = map (m !!!) $ filter (inMatrix base) $ map (c `sumPair`) $ map direction ds
                  calcNext coords m = case lowest of
                                        [] -> m
                                        ls -> setAt2 m coords $ minimum ls
                      where lowest = map (\v -> matrix !!! coords + v) $ neighbors coords m
          base = setAt2 (fill (x + 1, y + 1) 0) (x, y) (matrix !!! (x, y))

dims :: [[a]] -> (Int, Int)
dims matrix = (length (matrix !! 0), length matrix)

readMatrix :: Read a => FilePath -> IO [[a]]
readMatrix path = do
    contents <- readFile path
    let ls = map (splitOn ",") $ lines contents
    return $ map (map read) ls

main = do
    matrix <- readMatrix "p081_matrix.txt" :: IO [[Int]]

    let res = minPathSums [R, D] (dims matrix `sumPair` (-1, -1)) matrix
    mapM_ print res
    -- let res2 = map (matrix !!!) result

    -- print result
    -- print res2
