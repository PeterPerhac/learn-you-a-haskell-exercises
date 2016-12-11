import Data.List (isInfixOf)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Read, Eq)
data Digit = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Read, Bounded, Enum, Ord, Eq)
data Card = Card Digit Suit deriving (Eq)

instance Show Card where
    show (Card d s) = "The " ++ show d ++ " of " ++ show s

instance Ord Card where
    (Card d1 _) `compare` (Card d2 _) = d1 `compare` d2

betterCard :: Card -> Card -> Card
betterCard x y = max x y

class Hand a where
    play :: [a] -> Bool

instance Hand Card where
    play c = (Card Ace Spades) `elem` c

data Coin =  Heads | Tails deriving (Show, Read, Eq)

instance Hand Coin where
    play c = replicate 10 Heads `isInfixOf` c

throwCoins :: [(Int, Coin)] -> [Coin]
throwCoins [] = []
throwCoins ((i,c):tail) = replicate i c ++ throwCoins tail

-- play $ throwCoins [(2, Tails), (10, Heads), (1, Tails)]

