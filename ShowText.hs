module Story13 (showGame) where

import DataTypes
import GamePrint  

showGame :: GameState -> String
showGame = printGame