import Data.List.Split

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) e
    | x == e = xs
    | otherwise = x : remove xs e

passcodeWorks :: [Int] -> [Int] -> Bool
passcodeWorks [] [] = True
passcodeWorks [] _ = False
passcodeWorks _ [] = True
passcodeWorks code@(c:cs) request@(r:rs)
    | c == r = passcodeWorks cs rs
    | otherwise = passcodeWorks cs request

addToPasscode :: [Int] -> [Int] -> [Int]
addToPasscode _ [] = []
addToPasscode [] request = request
addToPasscode code@(c:cs) request@(r:[])
    | r `elem` code = code
    | c == r = code
    | otherwise = r : cs
addToPasscode code@(c:cs) request@(r:nr:rs)
    | c == r && nr `elem` cs = c : addToPasscode cs rs
    | c == nr = r : c : addToPasscode nextCsR rs
    | c == r = c : nr : addToPasscode nextCsNr rs
    | otherwise = c : addToPasscode cs request
    where nextCsR = remove cs r
          nextCsNr = remove cs nr

makePasscode :: [Int] -> [Int] -> [Int]
makePasscode code request
    | passcodeWorks code request = code
    | otherwise = addToPasscode code request

readIntLists :: FilePath -> IO [[Int]]
readIntLists path = do
    contents <- readFile path
    let fileLines = map (chunksOf 1) $ lines contents
    return $ map (map read) fileLines

-- testPasscode :: FilePath -> [Int] -> Bool
testPasscode path passcode = do
    lists <- readIntLists path
    mapM print $ map (\i -> show i ++ ": " ++ show (passcodeWorks passcode i)) lists

makePasscodes :: FilePath -> IO [Int]
makePasscodes path = do
    lists <- readIntLists path
    return $ foldl makePasscode [] lists
