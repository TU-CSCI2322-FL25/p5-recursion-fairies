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
    in (choosePlayerBest player childStates, curDepth)

choosePlayerBest :: Player -> [(Winner,Int)] -> Winner
choosePlayerBest player depthOut
  | Won player `elem` wins = Won player
  | Tie `elem` wins = Tie
  | otherwise = Won $ nextPlayer player
  where wins = [w | (w,_) <- depthOut]

bestMove :: GameState -> Move
bestMove state@(game, currPlayer, _) = let
    legalMoves = checkLegalMoves state
    resultingMoves = [(winner, (depth, move)) | move <- legalMoves, let (winner, depth) = whoWinDepth (makeMove state move) 0]
    sortedMoves = sortBy compareDepth resultingMoves
      where compareDepth (_,(d1,_)) (_,(d2,_)) = compare d1 d2
    -- resultingGames is a list of tuples of the form (winner of the game resulting from the move being played, move)
    winningTuple = lookup (Won currPlayer) sortedMoves
    in case winningTuple of
       Just (_, move) -> move
       Nothing -> let tieGame = lookup Tie sortedMoves
                    in case tieGame of
                       Just (_, move) -> move
                       Nothing   -> snd $ snd (head sortedMoves)