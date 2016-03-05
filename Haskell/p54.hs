import System.Directory

import qualified Data.Map.Lazy as Map
import Data.List

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord, Enum)

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Eq, Ord, Enum)

data Card = Card Value Suit
    deriving (Show, Eq)

data Hand = Hand [Card]
    deriving (Eq, Show)

data Ranking = HighCard Card
             | OnePair Value
             | TwoPair Value Value
             | ThreeOfAKind Value
             | Straight Value
             | Flush
             | FullHouse Value Value
             | FourOfAKind Value
             | StraightFlush Value
             | RoyalFlush
    deriving (Show, Eq, Ord)

instance Ord Card where
    (Card v1 s1) <= (Card v2 s2) = v1 <= v2

instance Ord Hand where
    a@(Hand aCards) <= b@(Hand bCards)
        | aRanking == bRanking = reverse aCards <= reverse bCards
        | otherwise = aRanking <= bRanking
        where aRanking = makeRanking a
              bRanking = makeRanking b

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = go xs xs
    where go (x:xs) (_:_:ys) = let (first,last) = go xs ys in (x:first, last)
          go (x:xs) [_] = ([x],xs)
          go (x:xs) []  = ([],x:xs)

removeAll :: Eq a => [a] -> a -> [a]
removeAll [] _ = []
removeAll (x:xs) e
    | e == x = removeAll xs e
    | otherwise = x : removeAll xs e

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

countDuplicates :: Eq a => [a] -> [(Int, a)]
countDuplicates [] = []
countDuplicates (x:xs) = (count (== x) xs + 1, x) : countDuplicates (removeAll xs x)

value :: Card -> Value
value (Card val _) = val

suit :: Card -> Suit
suit (Card _ s) = s

highCard :: Hand -> Ranking
highCard (Hand cards) = HighCard (maximum cards)

makeRanking :: Hand -> Ranking
makeRanking h@(Hand cards)
    | values == [Ten ..] && length suits == 1 = RoyalFlush
    | values == take (length values) [values !! 0 ..] && length suits == 1 = StraightFlush (values !! 0)
    | fst (kinds !! 0) == 4 = FourOfAKind (snd (kinds !! 0))
    | fst (kinds !! 0) == 3 && fst (kinds !! 1) == 2 = FullHouse (snd (kinds !! 0)) (snd (kinds !! 1))
    | length suits == 1 = Flush
    | values == take (length values) [values !! 0 ..] = Straight (values !! 0)
    | fst (kinds !! 0) == 3 = ThreeOfAKind (snd (kinds !! 0))
    | fst (kinds !! 0) == 2 && fst (kinds !! 1) == 2 = TwoPair (snd (kinds !! 0)) (snd (kinds !! 1))
    | fst (kinds !! 0) == 2 = OnePair (snd (kinds !! 0))
    | otherwise = highCard h
    where suits = nub $ map suit cards
          values = map value cards
          kinds = reverse $ sort $ countDuplicates values

cardValueMap = Map.fromList (zip (['2'..'9'] ++ ['T', 'J', 'Q', 'K', 'A']) ([Two .. Ten] ++ [Jack .. Ace]))
cardSuitMap = Map.fromList [('C', Club), ('D', Diamond), ('H', Heart), ('S', Spade)]

readCard :: String -> Card
readCard cardText = Card v s
    where v = case Map.lookup (cardText !! 0) cardValueMap of
                    Nothing -> Two
                    Just a -> a
          s = case Map.lookup (cardText !! 1) cardSuitMap of
                    Nothing -> Club
                    Just a -> a

readHand :: [String] -> Hand
readHand = Hand . sort . map readCard

readHandPair :: String -> (Hand, Hand)
readHandPair pairText = (readHand firstHalf, readHand secondHalf)
    where (firstHalf, secondHalf) = halve (words pairText)

readHandPairs :: FilePath -> IO [(Hand, Hand)]
readHandPairs path = do
    text <- readFile path
    let fLines = lines text

    return (map readHandPair fLines)

rankHandPairs :: [(Hand, Hand)] -> [Bool]
rankHandPairs = map (\(a, b) -> a >= b)

p54 :: FilePath -> IO Int
p54 path = do
    handPairs <- readHandPairs path
    let rankings = rankHandPairs handPairs
    return (length (filter (== True) rankings))

main = do
    result <- p54 "p54_hands.txt"
    print result
