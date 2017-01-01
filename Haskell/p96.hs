import Data.List
import Data.Maybe

import Lib ((!!!), setAt, setAt2)

data Sudoku = Sudoku [[Int]]
    deriving (Eq)

instance Show Sudoku where
    show (Sudoku board) = intercalate "\n" $ map show board

isSolution :: Sudoku -> Bool
isSolution sudoku@(Sudoku board) = all isValid $ board ++ transpose board ++ map concat (concat $ boxes sudoku)
    where isValid ns = all (`elem` ns) [1..9]

boxes :: Sudoku -> [[[[Int]]]]
boxes (Sudoku board) = [ [map (take 3 . drop (y * 3)) rows | y <- [0..2]] | x <- [0..2], let rows = take 3 $ drop (x * 3) board ]

getAllPossibleNumbers :: Sudoku -> [((Int, Int), [Int])]
getAllPossibleNumbers sudoku@(Sudoku board) = [((x, y), getAll (x, y)) | x <- [0..8], y <- [0..8]]
    where allBoxes = boxes sudoku
          getAll (x, y) = case board !!! (x, y) of
                            0 -> [1..9] \\ (row ++ column ++ concat box)
                            v -> [v]
            where row = board !! y
                  column = (transpose board) !! x
                  box = allBoxes !!! (x `div` 3, y `div` 3)

-- Recreates a board from a list of possibly values (like the one from getAllPossibleNumbers)
remakeBoard :: [((Int, Int), [Int])] -> Sudoku
remakeBoard = Sudoku . foldl repf base
    where base = replicate 9 $ replicate 9 0
          repf cur (pos, val) = setAt2 cur pos actualVal
            where actualVal = case val of
                                      [] -> error "No possible values."
                                      (x:[]) -> x
                                      _ -> 0

-- Get all the squares we can definitely figure out in this pass through
deductPass :: Sudoku -> Sudoku
deductPass = remakeBoard . getAllPossibleNumbers

-- Do deductPass repeatedly until there is no change
deductSolve :: Sudoku -> Sudoku
deductSolve sudoku = case nextSudoku == sudoku of
                        True -> sudoku
                        False -> deductSolve nextSudoku
    where nextSudoku = deductPass sudoku

-- Just try all the possible values
-- If we can't solve it, function will return Nothing
finalSolve :: Sudoku -> Maybe Sudoku
finalSolve inSudoku
    -- If any of the squares have no possibilities, clearly there is something wrong.
    | any (null . snd) allPossible = Nothing
    | otherwise = case nextUnknown of
                        -- There are no more unknowns, so we're probably done
                        Nothing -> case isSolution sudoku of
                                    True -> Just sudoku
                                    False -> Nothing
                        Just ((x,y), vals) -> case find isJust $ map finalSolve $ allNextSudoku (x,y) vals of
                                                Nothing -> Nothing
                                                Just Nothing -> Nothing
                                                -- A final check, just in case
                                                Just (Just final) -> case isSolution final of
                                                                True -> Just final
                                                                False -> Nothing
    where sudoku = deductSolve inSudoku
          allPossible = getAllPossibleNumbers sudoku
          nextUnknown = find (\(_, vals) -> length vals > 1) allPossible
          allNextSudoku (x, y) vals = allNextSudoku' vals
            where allNextSudoku' [] = []
                  allNextSudoku' (v:vs) = (remakeBoard $ nextPossible) : allNextSudoku' vs
                    where nextPossible = setAt allPossible (x * 3 + y) ((x,y),[v])

test :: Sudoku
test = Sudoku [[0,0,3,0,2,0,6,0,0],[9,0,0,3,0,5,0,0,1],[0,0,1,8,0,6,4,0,0],[0,0,8,1,0,2,9,0,0],[7,0,0,0,0,0,0,0,8],[0,0,6,7,0,8,2,0,0],[0,0,2,6,0,9,5,0,0],[8,0,0,2,0,3,0,0,9],[0,0,5,0,1,0,3,0,0]]

test2 :: Sudoku
test2 = Sudoku [[0,0,0,0,0,8,0,4,9],[0,8,0,0,0,4,5,7,6],[6,0,0,2,0,0,3,0,1],[7,9,0,5,2,0,0,0,0],[0,5,0,0,0,0,0,6,0],[0,0,0,0,4,9,0,1,5],[9,0,8,0,0,7,0,0,3],[1,4,7,9,0,0,0,5,0],[5,3,0,4,0,0,0,0,0]]

readSudoku :: [String] -> Sudoku
readSudoku ls = Sudoku $ map (map read) $ map (:[]) ls
