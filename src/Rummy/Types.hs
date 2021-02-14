{-# LANGUAGE NamedFieldPuns #-}

-- | Datatypes for playing Gin Rummy
module Rummy.Types where

import Cards
import Control.DeepSeq(NFData, rnf, rwhnf)

type PlayerId = String
-- | Is a bit more complex than expected as we need the info for making logs
data Play = Play
  { playId :: PlayerId
  , top :: Card
  , picked :: Card
  , draw :: Draw
  , act :: Action
  , memory :: String
  , finalHand :: [Card]
  }
type Trick = [Play]

data Meld =
      Deadwood Card            -- An unmelded card
    | Set3 Card Card Card      -- 3 cards of same rank different suit
    | Set4 Card Card Card Card -- 4 cards of same rank different suit
    | Straight3 Card Card Card -- 3 cards of same suit, sequential ranks
    | Straight4 Card Card Card Card -- 4 cards of same suit, sequential ranks
    | Straight5 Card Card Card Card Card -- 5 cards of same suit, sequential ranks

instance NFData Meld where
  rnf = rwhnf

data Draw = Stock | Discard
data Action = Action Act Card
data Act = Gin | Knock | Drop
  deriving(Show)

instance NFData Action where
  rnf = rwhnf

instance NFData Draw where
  rnf = rwhnf

-- Error management
data GameError = GameError PlayerError PlayerId

instance Show GameError where
    show (GameError err pid) = "Error: '" ++ pid ++ "' " ++ show err

data PlayerError = ReplayError
                 | InvalidCardError
                 | TimeError
                 | MemoryError
                 | CallError
                 | SetError
                 | StraightError
                 | SuitError
                 | KnockError
                 | GinError
                 | OmitError

-- | Default text for errors
instance Show PlayerError where
  show ReplayError = "Cannot play the last drawn card"
  show InvalidCardError = "Card played wasn't in player's hand"
  show TimeError = "Took too long to play"
  show MemoryError = "Memory is to large"
  show CallError = "Called on first turn"
  show SetError = "All cards in a set must be of the same rank"
  show SuitError = "All cards in a straight must be of the same suit"
  show StraightError = "All cards in a straight must be of consecutive ranks"
  show KnockError = "Knocked with more than 10 points in deadwood"
  show GinError = "Called gin with remaining deadwood"
  show OmitError = "Did not combine all cards into melds"

data Player = Player {
  playerId :: PlayerId,
  playFunc :: PlayFunc,
  actionFunc :: ActionFunc,
  meldFunc :: MeldFunc
}

instance Eq Player where
  Player{playerId=a} == Player{playerId=b} = a == b

instance Show Player where
  show Player{playerId} = "Player: " ++ show playerId

data Hand = Hand {
  owner :: Player,
  cards :: [Card]
}

data GameResult = GameResult {
  hands :: [HandResult],
  gameScore :: [GameScore],
  updatedPlayers :: [Player]
}

data HandResult = HandResult {
  tricks :: Trick,      -- ^ One play at a time
  scores :: [HandScore] -- ^ One score per player
}

type Score = Int

instance Show HandResult where
  show (HandResult _ scores) = show scores

data HandScore = HandScore {
  scoreId :: PlayerId,
  score :: Score
} deriving Show

data GameScore = GameScore {
  player :: Player,
  finalScore :: Score
}

instance Show GameScore where
  show (GameScore Player{playerId} score) = "Player: " ++ show playerId ++
    ", final score: " ++ show score

-- | Play function type.
--
-- A player receives the card he decided to draw (from discard or stock), her
-- hand and her memory. She then choses whether to Knock or Discard.
type PlayFunc
  = Card              -- ^ picked card
  -> (Score, Score)   -- ^ scores of (player, opponent) as of last round
  -> String           -- ^ the player's memory
  -> [Card]           -- ^ the player's hand (without new card)
  -> (Action, String) -- ^ the player's chosen card and new memory

-- | Action function type.
--
-- This function is called at the beginning of a turn before the player has to
-- form melds.
type ActionFunc
  = Card            -- ^ card on top of the discard pile
  -> (Score, Score) -- ^ scores of (player, opponent) as of last round
  -> Maybe String
  -- ^ player's memory, on first player turn in the first round it will be Nothing
  -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
  -> [Card]     -- ^ the player's hand
  -> (Draw, String) -- ^ which pile did the player chose to draw from and memory

-- | Meld function type.
--
-- Which melds to use for scoring.
type MeldFunc
  = (Score, Score) -- ^ scores of (player, opponent) as of last round
  -> String        -- ^ the player's memory
  -> [Card]        -- ^ cards in player's hand
  -> [Meld]        -- ^ elected melds
