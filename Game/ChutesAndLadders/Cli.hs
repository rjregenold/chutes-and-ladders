module Game.ChutesAndLadders.Cli where

import Game.ChutesAndLadders.Util (trim)

printWelcome = do
  printPadding
  pSpaceA "Welcome to Chutes and Ladders."
  pSpaceA "Please provide the following information to begin:"

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
  pSpaceB $ "Player " ++ show number ++ ", please enter your name."
  name <- getLine
  case trim name of
    x | not $ null x -> return x
    _ -> promptPlayerName number

promptTurn name = do
  putStrLn $ name ++ ", it's your turn!"
  putStrLn "Press [Enter] to spin the spinner."
  getLine

promptNumber :: String -> Int -> Int -> IO Int
promptNumber p mn mx = do
  putStrLn p
  number <- getLine
  case reads number :: [(Int, String)] of
    [(x, _)] | x `elem` [mn..mx] -> return x
    _ -> do
      promptNumber p mn mx

printWinner x = do
  pSpaceA $ x ++ " wins!!"
  pSpaceA "Thanks for playing."

printPadding = putStrLn "\n\n\n"

pSpaceB s = putStrLn $ "\n" ++ s
pSpaceA s = putStrLn $ s ++ "\n"
