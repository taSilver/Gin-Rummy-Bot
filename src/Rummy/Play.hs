-- | Module to play a round of Rummy.
--
-- Distribute 10 cards to each player, they then have to draw/discard every turn
-- until they have formed all their cards into Melds.
module Rummy.Play where

-- System
import System.Timeout
import Data.List
import Data.Maybe
import Control.DeepSeq

-- Tournament
import Cards
import Deck
import EitherIO

-- Game used
import Rummy.Rules
import Rummy.Types

-- Play game

-- | Play out a full game, then update the players.
playGame
  :: Score    -- ^ max score for the game
  -> [Player] -- ^ a list of players
  -> EitherIO GameError GameResult
playGame max_score players = do
  (GameResult played results _) <- playUntil max_score nilResult
  return $ GameResult played results []
  where
    nilScore = map (flip GameScore 0) players
    nilResult = GameResult [] nilScore players

-- | Play hands until we reach the @max_score@.
playUntil :: Score -> GameResult -> EitherIO GameError GameResult
playUntil max_score results
  | hasScore max_score results = return results
  | otherwise = playDeck results >>= playUntil max_score

-- | The game stops when:
--
--  - one player has over 'max_score' points; and,
--  - exactly one player has the lowest score.
hasScore :: Score -> GameResult -> Bool
hasScore max_score (GameResult _ scores _) =
  finalScore loser >= max_score && finalScore start < finalScore contender
  where
    sorted = sortOn finalScore scores
    loser = last sorted
    start = head sorted
    contender = head (tail sorted) -- There should be at least two players

-- | Shuffle a deck then start the game, keep track of the score.
playDeck :: GameResult -> EitherIO GameError GameResult
playDeck (GameResult previous results players) = do
  deck <- liftIO shuffledDeck
  played <- playHand deck prev results
  return $ GameResult
    (played: previous)
    (calculateScore results (scores played))
    players
  where
    -- It's really @safeHead@
    prev = case previous of
      [] -> []
      p: _ -> tricks p

calculateScore :: [GameScore] -> [HandScore] -> [GameScore]
calculateScore scores played = zipWith toScore
  (sortOn gameId scores)
  (sortOn scoreId played)
  where
    toScore (GameScore p current) (HandScore _ score) =
      GameScore p (current + score)

-- | Distribute a (shuffled) deck to the players and start the game.
--
-- TODO Opponent gets first card "free"?
playHand :: [Card] -> Trick -> [GameScore] -> EitherIO GameError HandResult
playHand deck prev scores = do
  let dealt = deal 10 2 deck
      -- First card of the stock becomes the discard
      (top: stock) = drop 20 deck
      players = map player scores
  order <- liftIO $ shuffleList players
  tricked <- playTricks (zipWith Hand order dealt) stock [top] prev [] scores
  tricks <- liftEither $ checkTricks tricked
  let final = take 2 tricks
      action = (act . head) tricks
      pids = map playId final
      -- Careful with the sorting order here, need to sort using the /last
      -- entry/ in order to have it in the right place.
      caller: opponent: _ = sortOn (((==) (last pids)) . playerId) players
  -- This section could be cleaner, but I feel it is easier to follow this way.
  called <- checkMelds caller scores (head final)
  opposed <- checkMelds opponent scores (last final)
  -- Calculate points
  let results = calculateHandScores action called opposed
  return $ HandResult tricks (zipWith HandScore pids results)

checkTricks :: Trick -> Either GameError Trick
checkTricks [] = error "Needs at least one Play"
checkTricks [Play{playId}] = Left $ GameError CallError playId
checkTricks [Play{playId}, _] = Left $ GameError CallError playId
checkTricks ts = Right ts

checkMelds
  :: Player
  -> [GameScore]
  -> Play
  -> EitherIO GameError [(PlayerId, Meld)]
checkMelds Player{meldFunc, playerId} scores final = do
  melds <- timeCall (meldFunc points (memory final)) playerId (finalHand final)
  let paired = zip (repeat playerId) melds
  liftEither $ mapM_ (uncurry validMeld) paired >> validHand final paired
  where
    points = getScores playerId scores

-- | Rotate hands to exchange the positions of players
rotate :: [a] -> [a]
  -- Sanity checking
rotate [] = error "Needs something to rotate"
  -- Actual function, this technically works with one element but probably
  -- should not.
rotate (h: hs) = hs ++ [h]

-- | Play out a round one hand at a time.
playTricks
  :: [Hand]
  -> [Card]
  -> [Card]
  -> Trick
  -> Trick
  -> [GameScore]
  -> EitherIO GameError Trick
playTricks _ [] _ _ tricks _ = return tricks
playTricks [player, other] stock discard prev tricks scores
  -- Stop the game after 100 actions per player
  | length tricks > 200 = return tricks
  | otherwise = do
    (play, stocked, discarded) <- playCard stock discard player prev tricks scores
    case act play of
      Action Drop _ -> playTricks [other, player{cards = finalHand play}]
                                  stocked
                                  discarded
                                  prev
                                  (play: tricks)
                                  scores
      _ -> return (play: tricks)
playTricks _ _ _ _ _ _ = error "Not enough players"

-- | Call the action and play functions of a player, updates the stock, discard
-- and player's hand.
playCard
  :: [Card]      -- ^ Stock pile
  -> [Card]      -- ^ Discard pile
  -> Hand        -- ^ Player hand
  -> Trick       -- ^ Previous tricks
  -> Trick       -- ^ Current tricks
  -> [GameScore] -- ^ Scores from previous round
  -> EitherIO GameError (Play, [Card], [Card])
-- TODO Better error handling
playCard _ [] _ _ _ _ = error "Discard cannot be empty"
playCard [] _ _ _ _ _ = error "Stock cannot be empty"
playCard (s: stock) (d: discard) hand prev current scores = do
  (choice, raw) <-
    timeCall (actionFunc d points memory (done current)) playerId handCards
  updated <- liftEither $ checkMemory raw
  let (picked, stocked, discarded) = case choice of
        Stock -> (s, stock, d: discard)
        Discard -> (d, s: stock, discard)
  (action, final) <- timeCall (playFunc picked points updated) playerId handCards
  state <- liftEither $ checkMemory final
  valid <- liftEither $ validPlay action picked playerId handCards
  return (Play playerId d picked choice action state (picked: delete valid handCards),
          stocked,
          valid: discarded)
  where
    (Hand Player{playerId, playFunc, actionFunc} handCards) = hand
    memory = firstJust (getMemory playerId current) (getMemory playerId prev)
    done xs = case xs of
      [] -> Nothing
      p: _ -> Just $ draw p
    points = getScores playerId scores
    checkMemory mem
      | length mem > (10 ^ (4 :: Int)) = Left $ GameError MemoryError playerId
      | otherwise = Right mem

-- | Left-biased choice on @Maybe@
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust _ y = y

getScores :: PlayerId -> [GameScore] -> (Score, Score)
getScores pid scores = (getScore (== pid) scores, getScore (/= pid) scores)

getScore :: (PlayerId -> Bool) -> [GameScore] -> Score
getScore cmp scores = fromMaybe 0 $
                      finalScore <$> find (cmp . playerId . player) scores

-- | Generic function for calling player functions with a timer
timeCall
  :: NFData b
  => ([Card] -> b)
  -> PlayerId
  -> [Card]
  -> EitherIO GameError b
timeCall func pid handCards = EitherIO $ do
  -- Careful, these are microsecs
  played <- timeout 1000000 $ return $!! func handCards -- Will force evaluation
  let timed = case played of
        Nothing -> Left $ GameError TimeError pid
        Just c -> Right c
  return timed

getMemory :: PlayerId -> Trick -> Maybe String
getMemory _ [] = Nothing
getMemory pid x = field x
  where
    field = fmap memory . listToMaybe . filter ((== pid) . playId)

-- | The player with the lowest deadwood score gets a bonus depending on their
-- last action plus the point difference in their deadwood.
calculateHandScores
  :: Action
  -> [(PlayerId, Meld)]
  -> [(PlayerId, Meld)]
  -> [Score] -- ^ (caller score, opponent score)
calculateHandScores (Action action _) caller opponent =
  case action of
    Gin -> [25 + tally opponent, 0] -- Should have 0 deadwood
    -- Undercut
    _ | tally caller >= tally opponent -> [0, 10 + tally caller - tally opponent]
      | otherwise -> [tally opponent - tally caller, 0]
  where
    tally = sum . map (cardPoints . snd)

-- Convenience conversion functions.

gameId :: GameScore -> PlayerId
gameId (GameScore Player{playerId} _) = playerId

handId :: Hand -> PlayerId
handId (Hand Player{playerId} _) = playerId
