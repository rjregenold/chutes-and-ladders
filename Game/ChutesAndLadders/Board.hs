module Game.ChutesAndLadders.Board
  (
    GameBoard,
    emptyGameBoard,
    renderBoardBoxes,
    Tile(..),
    tileAt
  ) where

import Data.List (sortBy, transpose)
import Data.List.Split (chunk)
import Data.Ord (comparing)
import Text.PrettyPrint.Boxes

import Game.ChutesAndLadders.Types

data Tile = Blank | Ladder Int | Chute Int
  deriving Eq

type Board = [Tile]

-- used to make visualizing the initial state of the board easier. the layout
-- board looks exactly like the actual game board.
newtype LayoutBoard = LayoutBoard Board

-- board representation used by the game engine.
newtype GameBoard = GameBoard Board

instance Show Tile where
  show Blank = "*"
  show (Ladder x) = "l|" ++ show x
  show (Chute x) = "c|" ++ show x

instance Boxable GameBoard where
  boxes (GameBoard b) = map (text . show) b

instance Show GameBoard where
  show b = renderBoardBoxes . boxes $ b

layoutBoard = LayoutBoard [
  Blank,     Blank,     Chute 77,  Blank,     Blank,     Chute 74,  Blank,     Chute 72,  Blank,     Blank,
  Blank,     Blank,     Blank,     Blank,     Blank,     Blank,     Chute 23,  Blank,     Blank,     Blank,
  Ladder 99, Blank,     Blank,     Blank,     Blank,     Blank,     Blank,     Blank,     Blank,     Ladder 90,
  Blank,     Chute 18,  Blank,     Chute 59,  Blank,     Blank,     Blank,     Blank,     Blank,     Blank,
  Blank,     Blank,     Blank,     Blank,     Chute 52,  Blank,     Blank,     Blank,     Blank,     Ladder 66,
  Blank,     Blank,     Blank,     Blank,     Blank,     Blank,     Chute 25,  Blank,     Chute 10,  Blank,
  Blank,     Blank,     Blank,     Blank,     Ladder 43, Blank,     Blank,     Blank,     Blank,     Blank,
  Ladder 41, Blank,     Blank,     Blank,     Blank,     Blank,     Blank,     Ladder 83, Blank,     Blank,
  Blank,     Blank,     Blank,     Blank,     Chute 5,   Blank,     Blank,     Blank,     Blank,     Blank,
  Ladder 37, Blank,     Blank,     Ladder 13, Blank,     Blank,     Blank,     Blank,     Ladder 30, Blank]

-- rotates a board using the given shift indexes. for example, given a list
--   [1, 2, 3, 4]
-- and the shift indexes
--   [3, 2, 1, 0]
-- this function would return
--   [4, 3, 2, 1]
rotateBoard :: [Int] -> [a] -> [a]
rotateBoard xs b = map snd . sortBy (comparing fst) $ zip xs b where

-- generates common shift indexes using the given test.
shiftIndexes :: (Int -> Bool) -> [Int]
shiftIndexes f = concatMap c b where
  a = (reverse . chunk 10) [0..99]
  b = zip [0..length a] a
  c x | f $ fst x = reverse $ snd x
      | otherwise = snd x

-- shift indexes to convert a LayoutBoard to a GameBoard
lbToGbXs = shiftIndexes (\x -> mod x 2 == 0)
-- shift indexes to convert a GameBoard to a LayoutBoard
gbToLbXs = shiftIndexes (\x -> mod x 2 == 1)
-- shift indexes to convert the LayoutBoard into a format suitable for
-- printing. the boxes lib requires printing each column and then joining them
-- horizontally. this shift index basically swaps rows and columns.
lbToSbXs = (concat . transpose . chunk 10) [0..99]

toGameBoard :: LayoutBoard -> GameBoard
toGameBoard (LayoutBoard b) = GameBoard $ rotateBoard lbToGbXs b

emptyGameBoard = toGameBoard layoutBoard

renderBoardBoxes :: [Box] -> String
renderBoardBoxes b = render $ hsep 1 left $ map (vcat left) rows where
    rows = chunk 10 showBoard
    showBoard = rotateBoard lbToSbXs . rotateBoard gbToLbXs $ b

tileAt (GameBoard b) i = b !! i
