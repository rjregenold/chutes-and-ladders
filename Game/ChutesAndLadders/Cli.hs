module Game.ChutesAndLadders.Cli where

import Data.List (intercalate)

import Game.ChutesAndLadders.Util (trim)

mkPrompt s = "(?) " ++ s
prompt s = (putStr . mkPrompt . concat) [s, ": "]

printWelcome = do
  printPadding
  pSpaceA "Welcome to Chutes and Ladders."
  pSpaceA "Please provide the following information to begin:"

promptGameMode = do
  (putStrLn . intercalate "\n") [
    "What type of game would you like to play?",
    "(1) Normal (press [Enter] to spin)",
    "(2) Enter Word (type simple word correctly to spin)"]
  promptNumber "Enter choice (1 or 2)" 1 2

promptNumPlayers = promptNumber p 2 4 where
  p = "Enter number of players (2-4)"

promptPlayerName :: Int -> IO String
promptPlayerName number = do
  prompt $ "Enter name for player " ++ show number
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
  prompt p
  number <- getLine
  case reads number :: [(Int, String)] of
    [(x, _)] | x `elem` [mn..mx] -> return x
    _ -> do
      promptNumber p mn mx

printWinner x = do
  pSpaceA $ x ++ " wins!!"
  pSpaceA "Thanks for playing."

printPaddingN n = putStr $ (concat . replicate n) "\n"
printPadding = printPaddingN 4

pSpaceB s = putStrLn $ "\n" ++ s
pSpaceA s = putStrLn $ s ++ "\n"
