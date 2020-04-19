module Cards (standardDeck, deal, scoreHand, scoreCrib, hisNibs) where
import Data.List


data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Enum, Ord)
instance Show Suit where
  show Hearts = "H" ; show Diamonds = "D" ; show Clubs  = "C" ; show Spades = "S"
instance Read Suit where
  readsPrec _ ('H':cs) = [(Hearts, cs)] ; readsPrec _ ('D':cs) = [(Diamonds, cs)]
  readsPrec _ ('C':cs) = [(Clubs, cs)] ;  readsPrec _ ('S':cs) = [(Spades, cs)]


data Rank = A | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K
  deriving (Eq, Enum, Ord)
instance Show Rank where
  show A   = "A" ; show R2  = "2" ;  show R3  = "3" ; show R4  = "4"
  show R5  = "5" ; show R6  = "6" ;  show R7  = "7" ; show R8  = "8"
  show R9  = "9" ; show R10 = "10" ; show J   = "J" ; show Q   = "Q" ; show K   = "K"
instance Read Rank where
  readsPrec _ ('A':cs) = [(A, cs)] ; readsPrec _ ('J':cs) = [(J, cs)]
  readsPrec _ ('Q':cs) = [(Q, cs)] ; readsPrec _ ('K':cs) = [(K, cs)]
  readsPrec _ ('2':cs) = [(R2, cs)] ; readsPrec _ ('3':cs) = [(R3, cs)]
  readsPrec _ ('4':cs) = [(R4, cs)] ; readsPrec _ ('5':cs) = [(R5, cs)]
  readsPrec _ ('6':cs) = [(R6, cs)] ; readsPrec _ ('7':cs) = [(R7, cs)]
  readsPrec _ ('8':cs) = [(R8, cs)] ; readsPrec _ ('9':cs) = [(R9, cs)]
  readsPrec _ ('1':'0':cs) = [(R10, cs)]


data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord)
instance Show Card where
  show (Card r s) = show r ++ show s
instance Read Card where
  readsPrec _ cs = [(Card r s, cs'')]
    where (r, cs') = head . readsPrec 0 $ cs
          (s, cs'') = head . readsPrec 0 $ cs'


makeCard :: String -> Card
makeCard = read
makeHand :: String -> Hand
makeHand = map read . words 


value :: Card -> Int
value (Card J _) = 10 ; value (Card Q _) = 10 ; value (Card K _) = 10 
value c = (fromEnum . rank $ c) + 1


type Starter = Card
type Deck = [Card]
type Hand = [Card]
type Set = [Card]


standardDeck = [ Card r s | s <- enumFrom Hearts, r <- enumFrom A ]


deal :: Int -> Deck -> (Hand, Deck)
deal = splitAt


fifteen :: Set -> Int
fifteen cs | (sum . map value $ cs) == 15 = 2
fifteen otherwise = 0


pair :: Set -> Int
pair [c1, c2] | rank c1 == rank c2 = 2
pair _ = 0


run :: Set -> Int
run cs | length cs >= 3 && run' cs = length cs
  where run' (c1:c2:cs) = v c2 == v c1 + 1 && run' (c2:cs)
        run' _ = True
        v = fromEnum . rank
run _ = 0


flush :: Hand -> Int
flush (c:cs) | all (\x -> suit x == s) cs = length cs + 1
  where s = suit c
flush _ = 0


hisNobs :: Starter -> Hand -> Int
hisNobs s = length . filter (\c -> suit c == suit s && rank c == J) 


hisNibs :: Starter -> Int
hisNibs (Card J _) = 2
hisNibs _ = 0


scoreCards :: Bool -> Starter -> Hand -> Int
scoreCards crib s h = 
  (sum . map scoreSet . sets $ (s:h)) 
    + flush (s:h)
    + if crib then 0 else flush h
    + hisNobs s h
  where scoreSet cs = sum . map (\f -> f cs) $ [fifteen, pair, run]
        sets = tail . Data.List.sort . sets'
        sets' [] = [[]]
        sets' (x:xs) = sets' xs ++ map (x:) (sets' xs)

scoreHand = scoreCards False
scoreCrib = scoreCards True


-- | Randomly shuffle a list
--   /O(N)/
{--
shuffle :: Deck -> IO Deck
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
--}
