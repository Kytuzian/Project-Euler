import Lib (countDuplicates, zipTo, remove)

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import System.IO

lower :: String -> String
lower = map toLower

wordList :: IO [String]
wordList = do
    contents <- Text.IO.readFile "words.txt"
    return $ map lower $ filter (not . null) $ splitOn "\n" $ Text.unpack contents

wordFreqs :: [String] -> [(String, String)]
wordFreqs = zipTo sort

possibleWords :: [(String, String)] -> [String] -> String -> [String]
possibleWords wfreqs ignoreWords word = filter (\w -> reverse w /= word) $ mainCheck
    where search = sort word
          len = length word
          possible = filter (\(w,_) -> length w == len && not (w `elem` ignoreWords)) wfreqs
          mainCheck = map fst $ filter ((== search) . snd) possible

fromDigits :: Integral a => [a] -> a
fromDigits (d:ds) = foldl (\a b -> 10 * a + b) d ds

toDigits :: Integral a => a -> [a]
toDigits n = reverse $ toDigits' n
    where toDigits' 0 = []
          toDigits' v = r : toDigits' d
            where (d, r) = v `divMod` 10

squares = map (^2) [1..]

isSquare n
    | (n `mod` 10) `elem` [2,3,7,8] = False
    | otherwise = (floor $ sqrt $ fromIntegral n)^2 == n

-- perms :: Int -> [a] -> [[a]]
perms 1 xs = map (:[]) xs
perms _ [x] = [[x]]
perms i xs = [x : p | x <- xs, p <- perms (i - 1) (remove xs x)]

squaresOfLength n = takeWhile (<(10^n)) $ map (^2) [len..]
    where len = ceiling $ sqrt (10^(n - 1))

isUnique xs = nub xs == xs

makeAllWordMaps :: String -> [Map.Map Char Integer]
makeAllWordMaps word = [curMap | values <- filter isUnique $ map toDigits $ squaresOfLength (length letters),
                                 let curMap = Map.fromList $ zip letters values,
                                 let value = transformUsing curMap word,
                                 isJust value,
                                 ((fromJust value) !! 0) /= 0, -- Leading zeroes not allowed
                                 isSquare $ fromDigits $ fromJust value]
    where letters = nub word

-- Uses a map to transform one list into another.
-- Returns Nothing if any of the keys are missing.
transformUsing :: Ord k => Map.Map k v -> [k] -> Maybe [v]
transformUsing curMap xs = sequence $ map (\k -> Map.lookup k curMap) xs

findAnagramWordPairs :: [(String, String)] -> [String] -> String -> [[(String, Integer)]]
findAnagramWordPairs wfreqs ignoreWords word = case anagrams of
                                                    [] -> []
                                                    [x] -> []
                                                    _ -> transformed
    where allWordMaps = makeAllWordMaps word
          anagrams = possibleWords wfreqs ignoreWords word
          transformed = [matches |  wordMap <- allWordMaps,
                            let matches = [(anagram, value) | anagram <- anagrams,
                                            let test = transformUsing wordMap anagram,
                                            isJust test,
                                            ((fromJust test) !! 0) /= 0, -- Leading zeroes not allowed
                                            let value = fromDigits $ fromJust test,
                                            isSquare value],
                            length matches > 1]

showMaxBy :: (Show a, Ord b) => (a -> b) -> [a] -> IO a
showMaxBy f (v:vs) = do
    print v
    showMaxBy' v vs
    where showMaxBy' cur [] = return cur
          showMaxBy' cur (x:xs)
            | f x > f cur = do
                print x
                showMaxBy' x xs
            | otherwise = showMaxBy' cur xs

main = do
    contents <- readFile "p098_words.txt"
    let testWords = map lower $ map read $ splitOn "," contents

    -- ws <- wordFreqs
    let wfreqs = wordFreqs testWords -- ws

    hSetBuffering stdout NoBuffering

    let allAnagramPairs = foldl (\completed word -> completed ++ (concat $ findAnagramWordPairs wfreqs (map fst completed) word)) [] testWords
    -- mapM_ print $ zip testWords allAnagramPairs
    -- mapM_ (mapM_ print) allAnagramPairs
    print allAnagramPairs
    -- mapM_ print allAnagramPairs

    showMaxBy snd allAnagramPairs
