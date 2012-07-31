module Game.ChutesAndLadders.Util
  (
    randInt,
    replaceElem,
    trim
  ) where

import Data.Char (isSpace)
import Text.PrettyPrint.Boxes
import System.Random

randInt n = getStdRandom (randomR (0, n))

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs i x = y ++ (x : z) where
  y = take i xs
  z = drop (i + 1) xs

trim = f . f where
  f = reverse . dropWhile isSpace
