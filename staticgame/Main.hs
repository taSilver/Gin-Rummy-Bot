module Main where

import Rummy.Play
import Rummy.Types
import EitherIO
import Control.Monad

import safe Player

-- This sets up a tournament with two instances of your player playing against
-- each other.  You can run different players against each other, but you'll
-- need to change the Module names of those players (don't forget to change the
-- module name back to "Player" when you submit your code)
players :: [Player]
players = [
    Player "2" Player.playCard Player.pickCard Player.makeMelds
  , Player "1" Player.playCard Player.pickCard Player.makeMelds
  ]

main :: IO ()
main = do
  played <- runEitherIO $ playGame 100 players
  case played of
    Right (GameResult hr scores _) -> do
      forM_ (reverse hr) print
      putStrLn "=============="
      forM_ scores print
    Left e -> print e
