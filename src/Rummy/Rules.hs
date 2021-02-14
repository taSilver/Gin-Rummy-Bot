-- | Rummy-specific rules
module Rummy.Rules where

import Data.List(sort)

import Deck
import Cards
import Rummy.Types
import EitherIO

-- | Card value is face value for number cards, 10 for face cards.
toPoints :: Card -> Int
toPoints (Card _ rank) | rank < Jack = fromEnum rank + 1
                       | otherwise = 10 -- Face cards are all worth 10

-- | The scoring in Rummy is:
--
--  - card value for Deadwood;
--  - nil for Melds.
cardPoints :: Meld -> Int
cardPoints (Deadwood c) = toPoints c
cardPoints _ = 0

-- There is no bidding in Rummy.
dealAndBid :: Int -> [Player] -> [Card] -> EitherIO PlayerError [Hand]
dealAndBid n players deck = return $ zipWith Hand players hands
  where
    hands = deal n (length players) deck

-- | Verify that the chosen card is a valid play:
--
--  - Cannot play the card just drawn (replay).
--  - Has to play a card in hand (invalid).
validPlay
  :: Action   -- ^ Card discarded
  -> Card     -- ^ Last drawn card
  -> PlayerId -- ^ Player
  -> [Card]   -- ^ Hand
  -> Either GameError Card
validPlay (Action _ card) drawn playerId handCards
  | card == drawn = Left $ GameError ReplayError playerId
  | card `notElem` handCards = Left $ GameError InvalidCardError playerId
  | otherwise = Right card

-- | Check that melds formed are valid:
--
--   - Sets are of the same rank.
--
--   - Straights are of same suit and consecutive ranks.
validMeld :: PlayerId -> Meld -> Either GameError Meld
validMeld playerId meld = case meld of
  Deadwood _ -> Right meld
  Set3 a b c -> toEither (sameRank a [b, c]) meld setError
  Set4 a b c d -> toEither (sameRank a [b, c, d]) meld setError
  _ -> checkStraight playerId meld
  where
    setError = GameError SetError playerId
    rank (Card _ r) = r
    sameRank = same rank

same :: Eq b => (t -> b) -> t -> [t] -> Bool
same f c xs = all (== f c) (map f xs)

checkStraight :: String -> Meld -> Either GameError Meld
checkStraight pid meld = case meld of
  Straight3 a b c -> go [a, b, c]
  Straight4 a b c d -> go [a, b, c, d]
  Straight5 a b c d e -> go [a, b, c, d, e]
  _ -> error "This function can only be called with straights"
  where
    go [] = error "Impossible to call with empty list!"
    go l@(x: xs) =
      toEither (sameSuit x xs) meld (GameError SuitError pid) >>
      toEither ((consecutive . ranked) l) meld (GameError StraightError pid)
    suit (Card s _) = s
    rank (Card _ r) = r
    sameSuit = same suit
    ranked = sort . map rank
    consecutive l = ((fromEnum . last) l - (fromEnum . head) l) == (length l - 1)

-- | Check that the full hand of melds is valid.
validHand :: Play -> [(PlayerId, Meld)] -> Either GameError [(PlayerId, Meld)]
validHand Play{playId, finalHand, act} melds =
  toEither (allCards laid) melds (GameError OmitError playId) >>
  toEither check melds (GameError err playId)
  where
    laid = map snd $ filter ((playId ==) . fst) melds
    deadwood = [x | Deadwood x <- laid]
    (Action action _) = act
    (check, err) = case action of
      Gin -> (null deadwood, GinError)
      Knock -> ((sum . map toPoints) deadwood <= 10, KnockError)
      Drop -> (True, undefined) -- No deadwood error when not the caller
    -- Works because there is a complete order on Cards
    allCards melded = sort finalHand == ((sort . concatMap combine) melded)
    combine (Deadwood c) = [c]
    combine (Set3 a b c) = [a, b, c]
    combine (Set4 a b c d) = [a, b, c, d]
    combine (Straight3 a b c) = [a, b, c]
    combine (Straight4 a b c d) = [a, b, c, d]
    combine (Straight5 a b c d e) = [a, b, c, d, e]

toEither :: Bool -> a -> b -> Either b a
toEither b v err
  | b = Right v
  | otherwise = Left err
