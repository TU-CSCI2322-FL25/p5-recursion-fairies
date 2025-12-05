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
