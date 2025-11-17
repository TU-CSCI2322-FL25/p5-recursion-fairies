module InputText (readGameState) where

import DataTypes
import System.IO

readGameState :: FilePath -> IO GameState
readGameState path = do
    conts <- readFile path
    let lns = lines conts
        currentPlayer = spotToPlayer $ parseSpot $ head lns
        loc = parseLocation (lns !! 1)
        boardLns = drop 2 lns
        board = map parseSubBoard boardLns
    if length board == 9 then return (board, currentPlayer, loc) else error "Invalid board length"

parseLocation [] = Nothing
parseLocation l = Just (read l)

parseSpot :: String -> Spot
parseSpot "X" = Full X
parseSpot "O" = Full O
parseSpot "E" = Emp
parseSpot s = error ("Invalid player: " ++ s)

parseSubBoard :: String -> SubBoard
parseSubBoard [] = error "Empty SubBoard line"
parseSubBoard [winner] = Complete $ Won $ spotToPlayer $ parseSpot [winner]
parseSubBoard ln =
    let pList = words ln
    in Incomplete $ map parseSpot pList