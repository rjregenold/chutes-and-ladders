module Player where

data Player = Player {
  number :: Int,
  character :: Char,
  currentIndex :: Int
} deriving (Show)
