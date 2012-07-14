module Game.ChutesAndLadders.Util
  (
    replaceElem,
    trim
  ) where

import qualified Data.Text as T

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs i x = y ++ (x : z) where
  y = take i xs
  z = drop (i + 1) xs

trim = T.unpack . T.strip . T.pack 
