data Direction = L | R | U | D

type Position = (Int, Int)

move :: Position -> Direction -> Position
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)

minPathSums :: Num a => [[a]] -> [[a]]
minPathSums vs = pathSum'
    where pathSum' (c:cs) = 
