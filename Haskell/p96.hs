import Data.List (transpose, find, (\\))
import Data.List.Split
import Data.Maybe

import Library.List ((!!!), setAt2)

type Board = [[Int]]

testBoard :: Board
testBoard = [[0,0,3,0,2,0,6,0,0],
             [9,0,0,3,0,5,0,0,1],
             [0,0,1,8,0,6,4,0,0],
             [0,0,8,1,0,2,9,0,0],
             [7,0,0,0,0,0,0,0,8],
             [0,0,6,7,0,8,2,0,0],
             [0,0,2,6,0,9,5,0,0],
             [8,0,0,2,0,3,0,0,9],
             [0,0,5,0,1,0,3,0,0]]

squares :: Board -> [[[Int]]]
squares rows = [[concat (map (take 3 . drop (x * 3)) $ take 3 $ drop (y * 3) rows) | y <- [0..2]] | x <- [0..2]]

isSolved :: Board -> Bool
isSolved rows = all isSolved' $ rows ++ transpose rows ++ (concat $ squares rows)
    where isSolved' row = all (`elem` row) [1..9]

possible :: Board -> (Int, Int) -> [Int]
possible rows (x, y) = [1..9] \\ ((rows !! y) ++ ((transpose rows) !! x) ++ square)
    where square = squares rows !!! (y `div` 3, x `div` 3)

solve :: Board -> Maybe Board
solve startBoard = solve' open startBoard
    where open = [(x, y) | y <- [0..8], x <- [0..8], startBoard !!! (x, y) == 0]
          solve' [] board = Just board
          solve' (p:ps) board =
              case possible board p of
                  [] -> Nothing
                  options -> let nextBoards = map (\option -> setAt2 board p option) options in
                    case find isJust $ map (solve' ps) nextBoards of
                        Nothing -> Nothing
                        Just Nothing -> Nothing
                        Just (Just b) -> Just b

readSudoku :: [String] -> Board
readSudoku ls = map (map read) $ map (chunksOf 1) ls

readSudokus :: FilePath -> IO [Board]
readSudokus path = do
    contents <- readFile path
    let puzzles = map tail $ chunksOf 10 $ lines contents
    return $ map readSudoku puzzles

fromDigits :: Integral a => [a] -> a
fromDigits (d:ds) = foldl (\a b -> 10 * a + b) d ds

main = do
    puzzles <- readSudokus "p096_sudoku.txt"
    -- print puzzles
    let solutions = map solve puzzles
    mapM_ (\(i, sol) -> putStrLn $ show i ++ "\n" ++ show sol) $ zip [1..] solutions
    let nums = map (take 3 . head . fromJust) solutions
    print nums
    print $ map fromDigits nums
    print $ sum $ map fromDigits nums
