module Game.ChutesAndLadders.Cli where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import System.IO (hFlush, stdout)

import Game.ChutesAndLadders.Types
import Game.ChutesAndLadders.Util (randInt, trim)

mkPrompt s = "(?) " ++ s
prompt s = (putStr . mkPrompt . concat) [s, ": "]
inlinePrompt s = do
  prompt s
  hFlush stdout
  getLine 

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
  name <- inlinePrompt $ "Enter name for player " ++ show number
  case trim name of
    x | not $ null x -> return x
    _ -> promptPlayerName number

promptTurn :: GameMode -> String -> IO String

promptTurn Normal name = do
  putStrLn $ name ++ ", it's your turn!"
  putStrLn "Press [Enter] to spin the spinner."
  getLine

promptTurn EnterWord name = do
  putStrLn $ name ++ ", it's your turn!"
  i <- randInt $ (length gameWords - 1)
  word <- return $ gameWords !! i
  promptWord word

promptWord word = let
  upperWord = map toUpper word 
  clean = map toLower . trim
  p = concat ["Type this word to spin the spinner - [", upperWord, "]"]
  in do
    input <- inlinePrompt p
    w <- return $ clean input
    case word == w of
      True -> return word
      False -> promptWord word

promptNumber :: String -> Int -> Int -> IO Int
promptNumber p mn mx = do
  number <- inlinePrompt p
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
