import Data.List

replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) e r
    | x == e = r : replace xs e r
    | otherwise = x : replace xs e r

checkMatch :: Eq a => a -> [a] -> [a] -> Bool
checkMatch _ [] [] = True
checkMatch _ _ [] = False
checkMatch _ [] _ = False
checkMatch ignore (a:as) (b:bs)
    | a == b || a == ignore = checkMatch ignore as bs
    | otherwise = False

p206Match = "1_2_3_4_5_6_7_8_9_0"

p206 = find (checkMatch '_' p206Match) $ map (show . (^ 2)) [lowerLimit, lowerLimit + 10..upperLimit]
    where upperLimit = ceiling $ sqrt $ read $ replace p206Match '_' '9'
          lowerLimit = (ceiling $ sqrt $ read $ replace p206Match '_' '0') - 1

main = do
    print $ p206
