module Deck where

import Cards
import Control.Monad
import Data.List
import System.Random

-- | We deal each hand from a new deck.  Hands can be any size as long as we
-- have enough cards in the deck for each player
data Deck = Deck {
  handSize :: Int,
  deck :: [Card]
}

-- | Deal n cards each to m players.
deal :: Int -> Int -> [Card] -> [[Card]]
deal n m = take m . map (take n) . iterate (drop n)

sortedDeck :: [Card]
sortedDeck = Card <$> [Spade ..] <*> [Ace ..]

shuffleList :: [a] -> IO [a]
shuffleList l = do
  i <- replicateM (length l) (randomIO :: IO Int)
  return $ map snd $ sortOn fst $ zip i l

shuffledDeck :: IO [Card]
shuffledDeck = shuffleList sortedDeck
