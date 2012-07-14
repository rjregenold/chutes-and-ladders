module Game.ChutesAndLadders.Cli where

import Game.ChutesAndLadders.Util (trim)

promptNumPlayers = do
  putStrLn "How many players (2-4)?"
  number <- getLine
  case reads number :: [(Int, String)] of
    [(x, _)] | x > 1 && x < 5 -> return x
    _ -> do
      putStrLn "Please enter a number between 2 and 4."
      promptNumPlayers

promptPlayerName :: Int -> IO String
promptPlayerName number = do
  putStrLn $ "Player " ++ show number ++ ", please enter your name."
  name <- getLine
  case trim name of
    x | not $ null x -> return x
    _ -> promptPlayerName number

promptTurn name = do
  putStrLn $ name ++ ", it's your turn!"
  putStrLn "Press any key to spin the spinner."
  getChar
