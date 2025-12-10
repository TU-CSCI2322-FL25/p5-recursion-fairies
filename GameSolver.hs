module GameSolver where

import DataTypes
import GameCode

import Data.List
import Data.Ord
import Debug.Trace

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

rateSubboard :: SubBoard -> Player -> Int
rateSubboard (Complete a) p = if a == Won p
                              then 5 else 0
rateSubboard brd@(Incomplete [a, b, c, d, e, f, g, h, i]) p = 
              let lines = [ [a, b, c], [d, e, f], [g, h, i],
                          [a, d, g], [b, e, h], [c, f, i],
                          [a, e, i], [c, e, g] ]
                  score lst = if Full (nextPlayer p) `elem` lst
                              then 0
                              else length $ filter (== Full p) lst
                  scored = sum $ map score lines 
              in rankScore scored
              where
                rankScore s
                  | s > 8 = 4
                  | s > 6 = 3
                  | s > 4 = 2
                  | s > 0 = 1
                  | otherwise  = 0

rateGame :: GameState -> Int
rateGame (brd@[a, b, c, d, e, f, g, h, i], p, m) = 
  case winnerOfBoard brd of
    Just (Won w) | w == p -> 110
    Just (Won w) -> -110
    Just Tie -> 0
    Nothing -> 
      let myScore = sum [rateSubboard sb p | sb <- brd]
          enemyScore = sum [rateSubboard sb (nextPlayer p) | sb <- brd]
      in myScore - enemyScore
rateGame _ = 0


canWin :: SubBoard -> Player -> Bool
canWin (Complete a) p = a == Won p
canWin (Incomplete [a, b, c, d, e, f, g, h, i]) p =
  let
    lines =
      [ [a, b, c], [d, e, f], [g, h, i],
      [a, d, g], [b, e, h], [c, f, i],
      [a, e, i], [c, e, g] ]
    isEnemy (Full q) = q == nextPlayer p
    isEnemy _ = False
    fixLine xs = not $ any isEnemy xs
  in any fixLine lines

whoMightWin :: GameState -> Int -> Int -> (Int, Move) -- uses (-1,-1) as a move that would never be used
whoMightWin game maxDepth curDepth =
  case checkWinner game of
    Just w ->
      let (board, player, _) = game
      in case w of
           Won p | p == player -> (110, (-1,-1))
           Won p -> (-110, (-1,-1))
           Tie -> (0, (-1,-1))
    Nothing ->
      if curDepth >= maxDepth
        then (rateGame game, (-1,-1))
        else 
          let moves = checkLegalMoves game
              (board, player, _) = game
          in if null moves
             then (0, (-1,-1))
             else pickBestLazy player moves (-111) (-1,-1)
  where
    pickBestLazy :: Player -> [Move] -> Int -> Move -> (Int, Move)
    pickBestLazy _ [] bestScore bestMove = (bestScore, bestMove)
    pickBestLazy player (mv:rest) bestScore bestMove =
      let newState = makeMove game mv
      in if newState == game
         then pickBestLazy player rest bestScore bestMove
         else
           case checkWinner newState of
             Just (Won p) | p == player -> (110, mv)
             Just (Won p) -> pickBestLazy player rest bestScore bestMove
             Just Tie -> 
               let newScore = 0
               in if newScore > bestScore
                  then pickBestLazy player rest newScore mv
                  else pickBestLazy player rest bestScore bestMove
             Nothing ->
               let (score, _) = whoMightWin newState maxDepth (curDepth + 1)
                   myScore = negate score
               in if myScore >= 110 
                  then (myScore, mv)
                  else if myScore > bestScore
                    then pickBestLazy player rest myScore mv
                    else pickBestLazy player rest bestScore bestMove