module GameSolver where

import DataTypes
import GameCode

import Data.List
import Data.Ord

whoWillWin :: GameState -> Winner
whoWillWin state = fst $ whoWinDepth state 0


-- whoMightWinDepth :: GameState -> Int -> Int -> (Int, Move, Int)
-- whoMightWinDepth state@(board, player, forced) maxDepth curDepth
--   | Just w <- checkWinner state = (w, curDepth)
--   | curDepth >= maxDepth = rateGame state
--   | otherwise =
--     let
--       children = map (makeMove state) (checkLegalMoves state)
--       childStates = map (\s -> whoWinDepth s (curDepth+1)) children
--     in chooseBest player childStates

-- chooseBest :: Player -> [(Winner,Int)] -> (Winner, Int)
-- chooseBest player outcomes
--   | any (\o -> fst o == Won player) outcomes = minimumBy compareDepth [out | out@(Won p, _) <- outcomes, p == player]
--   | any (\o -> fst o == Tie) outcomes = (Tie, 0)
--   | otherwise = maximumBy compareDepth [out | out@(Won p, _) <- outcomes, p /= player]
--   where
--       compareDepth (_, d1) (_, d2) = compare d1 d2

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
rateGame (brd@[a, b, c, d, e, f, g, h, i], p, m ) = 
                  let lines = [ [a, b, c], [d, e, f], [g, h, i],
                              [a, d, g], [b, e, h], [c, f, i],
                              [a, e, i], [c, e, g] ]
                      enemy = nextPlayer p
                      fixLines xs = canWin xs p
                      scored = sum [rateSubboard sb p | line <- lines, sb <- filter fixLines line]
                      otherScored = rateGame (brd, enemy, m)
                      winner = winnerOfBoard brd
                      
                  in case winner of
                       Just (Won w) | w==p -> 110
                       Just (Won w) | w==enemy -> -110
                       Just Tie -> 0
                       Nothing -> scored - otherScored


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
                      
-- whoMightWin :: GameState -> Int -> (Int, Move)
-- whoMightWin game 1 =
--   case checkLegalMoves game of
--     [] -> error "No legal moves" -- this is probably always going to happen? either way it should just be someone has won
--     moves ->
--       let scored = [(rateGame (makeMove game mv), mv) | mv <- moves]
--       in maximum scored
-- whoMightWin game depth = 
--   case checkWinner game of
--     Just w -> error "someone won" -- 
--     Nothing ->
--       let
--         moves = checkLegalMoves game
--         scored = [
--           let (s, _) = whoMightWin (makeMove game mv) (depth -1)
--           in (s, mv) | mv <- moves]
--       in maximum scored

whoMightWin :: GameState -> Int -> Int -> (Int, Move)
whoMightWin game maxDepth curDepth =
  case checkWinner game of
    Just (Won p) -> 
      let (board, player, _) = game
      in if p == player then (110, (-1,-1)) else (-110, (-1,-1))
    Just Tie -> (0, (-1,-1))
    Nothing ->
      if curDepth >= maxDepth
        then (rateGame game, (-1,-1))
        else
          let moves = checkLegalMoves game
              (board, player, _) = game
          in pickBestLazy player moves (-111) ((-1,-1))
      where
        pickBestLazy :: Player -> [Move] -> Int -> Move -> (Int, Move)
        pickBestLazy _ [] bestScore bestMove = (bestScore, bestMove)
        pickBestLazy player (mv:rest) bestScore bestMove =
          let (score, _) = whoMightWin (makeMove game mv) maxDepth (curDepth + 1)
              -- Negate score because it's from opponent's perspective
              myScore = negate score
          in
            --If we found a guaranteed win, stop searching
            if myScore >= 110 then (myScore, mv)
            --Otherwise, continue searching but keep track of best so far
            else if myScore > bestScore
              then pickBestLazy player rest myScore mv
              else pickBestLazy player rest bestScore bestMove