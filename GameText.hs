module GameText where

import DataTypes

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import System.IO

-- PRINTING BOARD FUNCS
--subBoard1 = Incomplete [Full X, Full O, Emp, Full X, Full O, Emp, Full O, Full X, Emp]
--subBoard2 = Complete $ Won X
--board = [subBoard1, subBoard1, subBoard2, subBoard1, subBoard1, subBoard2, subBoard2, subBoard2, subBoard1]
subBoardStr :: SubBoard -> (String, String, String)
subBoardStr (Complete w) = 
    ("           ", 
    "     " ++ winState ++ "     ",
    "           ")
    where
        winState = case w of
            Won X       -> "X"
            Won O       -> "O"
            Tie         -> "T"
            Unfinished  -> " "
subBoardStr (Incomplete lst) =
    let rows = chunksOf 3 (map showSpot lst)
        rowStrs = map (intercalate "|") rows
    in
    case rowStrs of
        [r1, r2, r3] -> (r1, r2, r3)

printGame :: GameState -> String
printGame (state, player, loc) = 
    printMainBoard state ++ "\nCurrent player is Player " ++ show player

printMainBoard :: Board -> String
printMainBoard board =
    let subStrs = map subBoardStr board
        mainRows = chunksOf 3 subStrs
        totalRow row =
            let (r1s, r2s, r3s) = unzip3 row
            in unlines [intercalate "||" r1s, intercalate "||" r2s, intercalate "||" r3s]
    in intercalate "=====================================" $ map totalRow mainRows

printSubBoard :: SubBoard -> String
printSubBoard (Complete w) = 
    "           \n" ++
    "     " ++ winState ++ "     \n" ++
    "           \n"
    where
        winState = case w of
            Won X       -> "X"
            Won O       -> "O"
            Tie         -> "T"
            Unfinished  -> " "
        
printSubBoard (Incomplete lst) =
    let rows = chunksOf 3 (map showSpot lst)
    in unlines [intercalate "|" r | r <- rows]

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

--I WILL DO: I will have it turn gamestate to String for textfile, then do textfile string to pretty print- i have been sick so one sec