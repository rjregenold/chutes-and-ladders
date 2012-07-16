module Game.ChutesAndLadders.Game
  (
    playGame
  ) where

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

getPlayers = do
  numPlayers <- promptNumPlayers
  makePlayers numPlayers

move :: GameBoard -> Move -> (Player, Maybe String)
move b (Move p s) = (,) p { currentIndex = i } m where
  oldIndex = currentIndex p
  newIndex = oldIndex + s
  (i,m) = case tileAt b newIndex of
    (Ladder x) -> (x, Just $ "You climbed a ladder to tile " ++ show x ++ "!")
    (Chute x) -> (x, Just $ "You slid down a chute to tile " ++ show x ++ ". :(")
    _ -> (newIndex, Nothing)

renderBoardAndPlayers :: GameBoard -> [Player] -> String
renderBoardAndPlayers b ps = renderBoardBoxes $ foldr f bs' ps' where
  bs' = boxes b
  ps' = playerBoxes ps
  f (i,x) acc | i >= 0 = replaceElem acc i x
              | otherwise = acc

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
    gameLoop b ps' (t + 1)

playGame = do
  printWelcome
  players <- getPlayers
  printPadding
  gameLoop emptyGameBoard players 0
