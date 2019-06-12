-- Problem 15
-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
--
--
-- How many such routes are there through a 20×20 grid?

import Lib (memoize)

data Grid = Grid Int Int
    deriving (Eq, Ord)

-- paths:: Grid -> Int
paths (Grid 0 0) = 0
paths (Grid x 0) = 1
paths (Grid 0 y) = 1
paths (Grid x 1) = x + 1
paths (Grid 1 y) = y + 1
paths (Grid x y)
  | (rightPossible (Grid x y) == True) && (downPossible (Grid x y) == True) = memPaths (right (Grid x y)) + memPaths (down (Grid x y))
  | otherwise = 0

memPaths = memoize paths

-- rightPossible :: Grid -> Bool
rightPossible (Grid x y)
  | x <= 0 = False
  | otherwise = True

-- downPossible :: Grid -> Bool
downPossible (Grid x y)
  | y <= 0 = False
  | otherwise = True

-- right :: Grid -> Grid
right (Grid x y) = Grid (x-1) y

-- down :: Grid -> Grid
down (Grid x y) = Grid x (y-1)

main = print $ paths $ Grid 10 20

-- paths (Grid x y)
--       | right_possible == False
--           | left_possible == False = 1
--           | otherwise = down Grid
--       | otherwise
--           | left_possible == False = right Grid
--           | otherwise = paths (right Grid) + paths (down Grid)
