module Game where

import Board (emptyGameBoard)
import Spinner (spinSpinner)

-- just do stupid stuff for now. spin the spinner a few times and show an
-- empty game board.
-- todo: actually implement this.
playGame = do
  spin1 <- spinSpinner
  spin2 <- spinSpinner
  spin3 <- spinSpinner
  putStrLn $ "good spins: " ++ (show [spin1, spin2, spin3])
  putStrLn $ show emptyGameBoard
