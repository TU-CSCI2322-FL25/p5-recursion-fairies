module GameText where

import DataTypes

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import System.IO

-- gamestate to printable format
subBoardStr :: SubBoard -> (String, String, String)
subBoardStr (Complete w) = 
    ("           ", 
    "     " ++ prettyWinner w ++ "     ",
    "           ")
subBoardStr (Incomplete lst) =
    let rows = chunksOf 3 $ map (\s -> " " ++ prettySpot s ++ " ") lst
        rowStrs@[r1, r2, r3] = map (intercalate "|") rows
    in (r1, r2, r3)

showGame :: GameState -> String
showGame (state, player, loc) = 
    showMainBoard state ++ "\nCurrent player is Player " ++ show player ++ "\nCurrent subboard is " ++ show loc

showMainBoard :: Board -> String
showMainBoard board =
    let subStrs = map subBoardStr board
        mainRows = chunksOf 3 subStrs
        totalRow row =
            let (r1s, r2s, r3s) = unzip3 row
            in unlines [intercalate "||" r1s, intercalate "||" r2s, intercalate "||" r3s]
    in intercalate "=====================================\n" $ map totalRow mainRows

-- gamestate to file format
gameStateOut :: GameState -> String
gameStateOut (board, player, cur) =
    p ++ "\n" ++ c ++ "\n" ++ b
    where
        p = show player
        c = case cur of
            Just n -> show n
            Nothing -> ""
        b = mainBoardOut board

mainBoardOut :: Board -> String
mainBoardOut board = let shownBoard = map subBoardOut board
    in unlines shownBoard

subBoardOut :: SubBoard -> String
subBoardOut (Complete w) = prettyWinner w
subBoardOut (Incomplete lst) = let shownSpots = map prettySpot lst
    in unwords shownSpots

-- file to string code
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
parseSubBoard [winner] = case winner of
    'T' -> Complete Tie
    _ -> Complete $ Won $ spotToPlayer $ parseSpot [winner]
parseSubBoard ln =
    let pList = words ln
    in Incomplete $ map parseSpot pList