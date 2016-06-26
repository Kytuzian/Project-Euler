import Data.List
import Data.List.Split

import Data.Ord

import Lib (zipTo, minimumIndexBy)

data Ring = Ring [Line]
data Line = Line [Node]
    deriving (Eq)

instance Show Ring where
    show (Ring ls) = intercalate "; " $ map show ls

instance Eq Ring where
    (Ring as) == (Ring bs) = any (`elem` as) bs

instance Show Line where
    show (Line ns) = intercalate "," $ map show ns

type Node = Int

ringLines :: Ring -> [Line]
ringLines (Ring ls) = ls

nodes :: Line -> [Node]
nodes (Line ns) = ns

sortLines :: [Line] -> [Line]
sortLines ls = drop i ls ++ take i ls
    where (i,_) = minimumIndexBy (head . nodes) ls

fromNodes :: [Node] -> Ring
fromNodes (p1:p2:p3:ps) = Ring newLines
    where newLines = sortLines ((Line [p1,p2,p3]):(fromNodes' p3 ps))
          fromNodes' _ [] = []
          fromNodes' prev (a:[]) = [Line [a,prev,p2]]
          fromNodes' prev (a:b:bs) = (Line [a,prev,b]) : fromNodes' b bs

checkRing :: Ring -> Bool
checkRing (Ring ls@(l:_)) = all ((== check) . sum . nodes) ls
    where check = sum $ nodes l

digitString :: Ring -> String
digitString (Ring ls) = foldl (++) "" $ map (foldl (++) "" . map show . nodes) ls

main = do
    let ns = permutations [1..10]
    let res = filter (checkRing . fromNodes) ns
    let finalRes = nub $ map fromNodes $ sortBy (comparing (sum . take 3)) res
    mapM_ print $ zipTo digitString finalRes
