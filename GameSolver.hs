module GameSolver where

import DataTypes
import GameCode

import Data.List

whoWillWin :: GameState -> Winner
whoWillWin state = fst $ whoWinDepth state 0

whoWinDepth :: GameState -> Int -> (Winner, Int)
whoWinDepth state@(board, player, forced) curDepth
  | Just w <- checkWinner state = (w, curDepth)
  | otherwise =
    let
      children = map (makeMove state) (checkLegalMoves state)
      childStates = map (\s -> whoWinDepth s (curDepth+1)) children
    in choosePlayerBest player childStates

choosePlayerBest :: Player -> [(Winner,Int)] -> (Winner, Int)
choosePlayerBest player outcomes
  | any (\o -> fst o == Won player) outcomes = minimumBy compareDepth [out | out@(Won p, _) <- outcomes, p == player]
  | any (\o -> fst o == Tie) outcomes = (Tie, 0)
  | otherwise = maximumBy compareDepth [out | out@(Won p, _) <- outcomes, p /= player]
  where
      compareDepth (_, d1) (_, d2) = compare d1 d2

bestMove :: GameState -> Move
bestMove state =
    let moves = checkLegalMoves state
        scored = [(whoWinDepth (makeMove state mv) 0, mv) | mv <- moves]
    in snd (minimumBy compareScore scored)
  where
    -- smaller depth wins if winner is same
    compareScore ((w1,d1),_) ((w2,d2),_) =
        compare (priority w1, d1) (priority w2, d2)
    priority (Won _) = 0
    priority Tie     = 1

rateSubboard :: SubBoard -> Player -> Maybe Int
rateSubboard (Complete a) p = if a == Won p
                              then 5 else 0
rateSubboard brd@(Incomplete [a, b, c, d, e, f, g, h, i]) p = 
              let lines    =  [[a, b, c], [d, e, f], [g, h, i]
                               [a, d, g], [b, e, h], [c, f, i]
                               [a, e, i], [c, e, g]]
                  score lst = if (Full (nextPlayer p)`elem` lst)
                              then 0
                              else length $ filter (\x -> x == Full p) lst
                  scored = sum $ map score lines 
              in case scored of | scored > 7 = 4
                                | scored > 4 = 3
                                | scored > 1 = 2
                                | scored > 0 = 1

rateGame :: Game -> Int
rateGame (brd@[a, b, c, d, e, f, g, h, i], p, m ) = 
                  let lines    =  [[a, b, c], [d, e, f], [g, h, i]
                                  [a, d, g], [b, e, h], [c, f, i]
                                  [a, e, i], [c, e, g]]
                      fixLines xs = if all canWin xs
                      scored = map rateSubboard $ filter fixLines lines
                      otherScored = rateGame (brd, (nextPlayer p), m)
                      winner = winnerOfBoard brd 
                  in case winner of
                       Just Won p -> 50
                       Just Won (nextPlayer p) -> -50
                       Just Tie -> 0
                       Nothing -> scored - otherScored


canWin :: SubBoard -> Player -> Bool
canWin (Complete a) p = if a == (Won p) then True else False
canWin (Incomplete brd@[a, b, c, d, e, f, g, h, i]) =
                  let lines   = [[a, b, c], [d, e, f], [g, h, i]
                                 [a, d, g], [b, e, h], [c, f, i]
                                 [a, e, i], [c, e, g]]
                      isEnemy a = if a == Won (nextPlayer p) then True else False
                      fixLine xs a = if any (map isEnemy xs) then False else True
                  in  length (filter fixLine lines) > 0  
                      