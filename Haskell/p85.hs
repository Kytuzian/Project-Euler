import Lib (combinationElements, triangleNumber, intQuadratic)

import Data.List

data Rectangle = Rectangle Int Int
    deriving (Show, Ord, Eq)

makeRectangle :: [Int] -> Rectangle
makeRectangle (a:b:_) = Rectangle a b

rectangleContainsN :: Rectangle -> Rectangle -> Int
rectangleContainsN (Rectangle w1 h1) (Rectangle w2 h2) = (w1 - w2 + 1) * (h1 - h2 + 1)

smallerRectangles :: Rectangle -> Int
smallerRectangles r@(Rectangle w h) = sum $ map (rectangleContainsN r . makeRectangle) rectangleDims
    where rectangleDims = combinationElements [[1..w], [1..h]]

test :: Rectangle -> Int
test (Rectangle w h) = triangleNumber w * triangleNumber h

p85 limit = snd $ last $ sort $ [(test rect, rect) | w <- [1..round $ sqrt $ fromIntegral limit], h <- [1..round $ sqrt $ fromIntegral limit], rect <- [Rectangle w h], test rect < limit]
