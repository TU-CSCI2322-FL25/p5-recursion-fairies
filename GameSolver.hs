module GameSolver where

import DataTypes
import GameCode
import GamePrint
import InputText

bestMove :: GameState -> Move
bestMove (game, currPlayer, l) = let
    legalMoves = checkLegalMoves (game, currPlayer, l)
    resultingGames = [(whoWillWin (makeMove (game, currPlayer, l) move), move) | move <- legalMoves]
    -- resultingGames is a list of tuples of the form (winner of the game resulting from the move being played, move)
    winningGame = lookup (Won currPlayer) resultingGames
    in case winningGame of
       Just move -> move
       Nothing   -> let tieGame = lookup Tie resultingGames
                    in case tieGame of
                       Just move -> move
                       Nothing   -> snd (head resultingGames)