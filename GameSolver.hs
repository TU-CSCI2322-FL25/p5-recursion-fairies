module GameSolver where

import DataTypes
import GameCode
import GamePrint
import InputText

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