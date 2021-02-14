-- | Implement the card-related types and helper functions
module Cards where

import Control.DeepSeq

-- | The four base suits
data Suit = Spade | Club | Diamond | Heart
  deriving (Eq, Ord, Enum, Bounded)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
  deriving (Eq, Ord, Enum, Bounded)

-- | A Card is a suit and a rank
data Card = Card Suit Rank
  deriving (Eq)

instance NFData Card where
  rnf = rwhnf

instance Ord Card where
  compare (Card s1 r1) (Card s2 r2) | s1 == s2 = compare r1 r2
                                    | otherwise = compare s1 s2
