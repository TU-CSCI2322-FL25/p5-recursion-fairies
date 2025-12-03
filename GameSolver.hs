module GameSolver where

import DataTypes
import GameCode

whoWillWin :: GameState -> Winner
whoWillWin state@(board, player, forced)
  | Just w <- checkWinner state = w
  | otherwise =
    let
      children = map (makeMove state) (checkLegalMoves state)
      childStates = map whoWillWin children
    in choosePlayerBest player childStates

bestMove :: GameState -> Move
bestMove state@(game, currPlayer, _) = let
    legalMoves = checkLegalMoves state
    resultingGames = [(whoWillWin (makeMove state move), move) | move <- legalMoves]
    -- resultingGames is a list of tuples of the form (winner of the game resulting from the move being played, move)
    winningGame = lookup (Won currPlayer) resultingGames
    in case winningGame of
       Just move -> move
       Nothing   -> let tieGame = lookup Tie resultingGames
                    in case tieGame of
                       Just move -> move
                       Nothing   -> snd (head resultingGames)