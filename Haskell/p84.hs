import Lib (rollDice)

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import System.Random

import System.ProgressBar

type Board = [String]
type Game = (Board, Map.Map String Int, Int, Int)

communityCards = ["GO", "JAIL"] ++ replicate 14 "NONE"
chanceCards = ["GO", "JAIL", "C1", "E3", "H2", "R1", "RN", "RN", "UN", "BACK3"] ++ replicate 6 "NONE"

monopoly = ["GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL",
            "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP",
            "E1", "CH2", "E2", "E3", "R3", "F1", "F2", "U2", "F3", "G2J",
            "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2"]

newGame s = (monopoly, Map.empty, 0, s)

lpad :: Int -> a -> [a] -> [a]
lpad i e xs = replicate (i - length xs) e ++ xs

randInt :: (Random a, Integral a) => a -> a -> IO a
randInt a b = getStdRandom $ randomR (a, b)

randIndex :: [a] -> IO Int
randIndex xs = randInt 0 (length xs - 1)

rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate i (x:xs) = rotate (i - 1) (xs ++ [x])

rotateBack :: Int -> [a] -> [a]
rotateBack 0 xs = xs
rotateBack i xs = rotateBack (i - 1) (last xs : init xs)

moveTo :: Eq a => a -> [a] -> [a]
moveTo e res@(x:xs)
    | e == x = res
    | otherwise = moveTo e (xs ++ [x])

doMove :: IO Game -> IO Game
doMove game = do
    (board, freq, doubles, s) <- game
    move1 <- 1 `rollDice` s
    move2 <- 1 `rollDice` s

    let moves = move1 + move2

    let newDoubles = case move1 == move2 of
                        True -> doubles + 1
                        False -> 0

    let newBoard = case newDoubles > 2 of
                        True -> moveTo "JAIL" board
                        False -> rotate moves board

    finalBoard <- handleSquare newBoard

    let newFreq = Map.insertWith (+) (head finalBoard) 1 freq

    return (finalBoard, newFreq, newDoubles, s)
    where squareType name
            | "CH" `isPrefixOf` name = "chance"
            | "CC" `isPrefixOf` name = "community"
            | name == "G2J" = "G2J"
            | otherwise = ""
          handleSquare board = do
              chanceCardI <- randIndex chanceCards
              communityCardI <- randIndex communityCards
              let chanceCard = chanceCards !! chanceCardI
              let communityCard = communityCards !! communityCardI

              return $ case squareType $ head board of
                          "chance" -> handleCard chanceCard board
                          "community" -> handleCard communityCard board
                          "G2J" -> moveTo "JAIL" board
                          _ -> board
          handleCard card board
            | card `elem` board = moveTo card board
            | "BACK" `isPrefixOf` card = rotateBack (read (drop 4 card)) board
            | "N" `isSuffixOf` card = case find ((card !! 0 : []) `isPrefixOf`) board of
                                        Nothing -> board
                                        Just v -> moveTo v board
            | otherwise = board

getSquareFreq :: Map.Map String Int -> [(String, Float)]
getSquareFreq freq = sortBy (comparing snd) $ map (\(s, v) -> (s, fromIntegral v / total * 100)) $ Map.toList freq
    where total = fromIntegral $ sum $ map snd $ Map.toList freq

moveN :: Int -> Int -> IO Game -> IO Game
moveN s 0 game = game
moveN s i game = do
    newGame@(board, freq, prevMoves, _) <- doMove game
    let n = show $ sum $ map snd $ Map.toList freq
    case i `mod` 100 of
        0 -> progressBar (msg n) percentage 80 (fromIntegral (s - i)) (fromIntegral s)
        _ -> putStr ""
    moveN s (i - 1) (return newGame)

-- doIt :: Int -> IO [(String, Float)]
doIt n = do
    (board, freq, prevMoves, s) <- moveN n n (return $ newGame 4)
    -- return $ sortBy (comparing snd) $ Map.toList freq
    let highest = map fst $ take 3 $ reverse $ getSquareFreq freq
    let result = catMaybes $ map (\h -> findIndex (== h) monopoly) highest
    return $ (foldl1 (++) $ map (lpad 2 '0' . show) $ result, result)

main = do
    res <- doIt 50000
    print res
