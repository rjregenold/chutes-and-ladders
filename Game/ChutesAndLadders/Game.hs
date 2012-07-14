module Game.ChutesAndLadders.Game
  (
    playGame
  ) where

import Game.ChutesAndLadders.Board
import Game.ChutesAndLadders.Cli
import Game.ChutesAndLadders.Player
import Game.ChutesAndLadders.Spinner (spinSpinner)
import Game.ChutesAndLadders.Util (replaceElem)

type Turn = Int
type Steps = Int

data Move = Move Player Steps deriving Show

getPlayers = do
  numPlayers <- promptNumPlayers
  makePlayers numPlayers

move :: GameBoard -> Move -> Player
move b (Move p s) = p { currentIndex = newIndex } where
  oldIndex = currentIndex p
  newIndex = oldIndex + s

gameLoop :: GameBoard -> [Player] -> Turn -> IO ()
gameLoop b ps t = 
  let i = mod t $ length ps
      p = (!!) ps i
  in do
    promptTurn $ name p
    spin <- spinSpinner
    putStrLn $ "You spun a " ++ show spin ++ "!\n"
    p' <- return $ move b $ Move p spin
    ps' <- return $ replaceElem ps i p'
    print b
    gameLoop b ps' (t + 1)

playGame = do
  players <- getPlayers
  gameLoop emptyGameBoard players 0
