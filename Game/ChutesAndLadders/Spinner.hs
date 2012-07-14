module Game.ChutesAndLadders.Spinner
  (
    spinSpinner
  ) where

import System.Random

spinSpinner :: IO Int
spinSpinner = getStdRandom (randomR (1, 6))
