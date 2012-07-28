module Game.ChutesAndLadders.Game
  (
    playGame
  ) where

import Data.List (find)
import qualified Data.Traversable as DT (traverse)

import Game.ChutesAndLadders.Board
import Game.ChutesAndLadders.Cli
import Game.ChutesAndLadders.Player
import Game.ChutesAndLadders.Spinner (spinSpinner)
import Game.ChutesAndLadders.Types
import Game.ChutesAndLadders.Util (replaceElem)

type Turn = Int
type Steps = Int

data Move = Move Player Steps

player (Move p _) = p

getPlayers = do
  numPlayers <- promptNumPlayers
  makePlayers numPlayers

move :: GameBoard -> Move -> (Player, Maybe String)
move b m = (,) p { currentIndex = i } msg where
  p = player m
  idx = newIndex b m
  (i,msg) = case tileAt b idx of
    (Ladder x) -> (x, Just $ "You climbed a ladder to tile " ++ show x ++ "!")
    (Chute x) -> (x, Just $ "You slid down a chute to tile " ++ show x ++ ". :(")
    _ -> (idx, Nothing)

isValidMove :: GameBoard -> Move -> Bool
isValidMove b m = (propIndex b m) < (numTiles b)

-- proposed index
propIndex :: GameBoard -> Move -> Int
propIndex b (Move p s) = currentIndex p + s

newIndex b m | isValidMove b m = propIndex b m
newIndex _ (Move p _) = currentIndex p

renderBoardAndPlayers :: GameBoard -> [Player] -> String
renderBoardAndPlayers b ps = renderBoardBoxes $ foldr f bs' ps' where
  bs' = boxes b
  ps' = playerBoxes ps
  f (i,x) acc | i >= 0 = replaceElem acc i x
              | otherwise = acc

checkWinner :: GameBoard -> [Player] -> Maybe Player
checkWinner b ps = find f ps where
  f p = currentIndex p == (numTiles b - 1)

gameLoop :: GameBoard -> [Player] -> Turn -> IO ()
gameLoop b ps t = 
  let i = mod t $ length ps
      p = ps !! i
  in do
    promptTurn $ name p
    spin <- spinSpinner
    putStrLn $ "You spun a " ++ show spin ++ "."
    (p', msg) <- return $ move b $ Move p spin
    DT.traverse putStrLn msg
    ps' <- return $ replaceElem ps i p'
    pSpaceB $ renderBoardAndPlayers b ps'
    printPadding
    case checkWinner b ps' of
      Just p -> printWinner $ name p
      Nothing -> gameLoop b ps' (t + 1)

playGame = do
  printWelcome
  players <- getPlayers
  printPadding
  gameLoop emptyGameBoard players 0
