module Game.ChutesAndLadders.Player where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Text.PrettyPrint.Boxes

import Game.ChutesAndLadders.Cli (promptNumber, promptPlayerName)
import Game.ChutesAndLadders.Types
import Game.ChutesAndLadders.Util

data Player = Player {
  number :: Int,
  name :: String,
  currentIndex :: Int,
  character :: String
}

instance Boxable Player where
  boxes p = [text . show $ p]

instance Show Player where
  show p = character p

characters = ["♘", "☃", "☺", "✿", "❤", "☼", "☁", "✝", "❀", "★"]

-- groups players by currentIndex
groupPlayers ps = groupBy f ps' where
  ps' = sortBy (comparing currentIndex) ps
  f = (==) `on` currentIndex

-- filters nullBox boxes out of a collection of boxes
noNulls :: [Box] -> [Box]
noNulls xs = filter f xs where
  f x = not $ and [cols x == 0, rows x == 0]

makePlayers :: Int -> IO [Player]
makePlayers count = mapM playerInfo [1..count] where
  playerInfo x = do
    name <- promptPlayerName x
    char <- selectCharacter characters
    return Player { number = x, name = name, currentIndex = -1, character = char }

selectCharacter :: [String] -> IO String
selectCharacter cs = 
  let mx = length cs
      cs' = concatMap f $ zip [1..mx] cs
      f (i, x) = concat [show i, ") ", x, "  "]
      prompt = "Please choose a character:\n" ++ cs'
  in do
    x <- promptNumber prompt 1 mx
    return $ cs !! (x-1)

playerBoxes :: [Player] -> [(Int, Box)]
playerBoxes ps = map f ps' where
  ps' = groupPlayers ps
  f x = foldr g (0, nullBox) x
  g x acc = (currentIndex x, punctuateH left (char '|') . noNulls $ boxes x ++ [snd acc])
