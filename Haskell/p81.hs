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
    where minPathSums' cur = nexts --foldl (\prev coords -> calcNext coords prev) cur nexts
            where nexts = sortBy (comparing (distance (x, y))) allCoords
                  allCoords = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]]
                  neighbors c m = map (m !!!) $ filter (inMatrix base) $ map (c `sumPair`) $ map direction ds
                  calcNext coords m = case lowest of
                                        [] -> m
                                        ls -> setAt2 m coords $ minimum ls
                      where lowest = map (\v -> matrix !!! coords + v) $ neighbors coords m
          base = setAt2 (fill (dims matrix) 0) (x, y) (matrix !!! (x, y))
          (width, height) = dims matrix

dims :: [[a]] -> (Int, Int)
dims matrix = (length (matrix !! 0), length matrix)

readMatrix :: Read a => FilePath -> IO [[a]]
readMatrix path = do
    contents <- readFile path
    let ls = map (splitOn ",") $ lines contents
    return $ map (map read) ls

p82 = do
    matrix <- readMatrix "p81_test.txt" :: IO [[Int]]

    let (width, height) = dims matrix

    -- Get the min path sums for all of the rows in the last column
    let results = [minPathSums [U, R, D] (width - 1, y) matrix | y <- [0..height - 1]]
    return $ results
    -- let minFirstColumns = map head $ map transpose results

    -- return $ minFirstColumns

p81 = do
    matrix <- readMatrix "p081_matrix.txt" :: IO [[Int]]

    let res = minPathSums [R, D] (dims matrix `sumPair` (-1, -1)) matrix
    mapM_ print res
    -- let res2 = map (matrix !!!) result

    -- print result
    -- print res2
