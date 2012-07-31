module Game.ChutesAndLadders.Types where

import Text.PrettyPrint.Boxes

type Turn = Int
type Steps = Int
type Character = String

data GameMode = Normal | EnterWord

data Move = Move Player Steps

data Player = Player {
  number :: Int,
  name :: String,
  currentIndex :: Int,
  character :: String
}

class Boxable a where
  boxes :: a -> [Box]

instance Boxable Player where
  boxes p = [text . show $ p]

instance Show Player where
  show p = character p

gameWords = ["a", "and", "away", "big", "blue", "can", "come", "down", "find",
  "for", "funny", "go", "help", "hers", "i", "in", "is", "it", "jump",
  "little", "look", "make", "me", "my", "not", "one", "play", "red", "run",
  "said", "see", "the", "three", "to", "two", "up", "we", "yellow"]
