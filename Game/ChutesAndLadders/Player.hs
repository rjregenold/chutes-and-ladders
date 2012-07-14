module Game.ChutesAndLadders.Player where

import Game.ChutesAndLadders.Cli (promptPlayerName)

data Player = Player {
  number :: Int,
  name :: String,
  currentIndex :: Int
} deriving (Show)

makePlayers :: Int -> IO [Player]
makePlayers count = mapM playerInfo [1..count] where
  playerInfo x = do
    name <- promptPlayerName x
    return Player { number = x, name = name, currentIndex = -1 }
