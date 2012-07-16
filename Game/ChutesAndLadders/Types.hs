module Game.ChutesAndLadders.Types where

import Text.PrettyPrint.Boxes

class Boxable a where
  boxes :: a -> [Box]
