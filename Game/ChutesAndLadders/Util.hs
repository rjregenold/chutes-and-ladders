module Game.ChutesAndLadders.Util
  (
    replaceElem,
    trim
  ) where

import Data.Text (pack, strip, unpack)

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs i x = y ++ (x : z) where
  y = take i xs
  z = drop (i + 1) xs

trim = unpack . strip . pack 
