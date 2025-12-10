module Main where

import DataTypes
import GameCode
import GameSolver
import System.CPUTime
import Text.Printf
import Control.Exception (evaluate)

-- Timing utility
timeIt :: String -> IO a -> IO a
timeIt label action = do
    putStr $ label ++ "... "
    start <- getCPUTime
    result <- action
    _ <- evaluate result  -- Force evaluation
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "%.3f sec\n" (diff :: Double)
    return result

-- Test game states
emptyGame :: GameState
emptyGame = (replicate 9 (Incomplete (replicate 9 Emp)), X, Nothing)

-- One move away from winning sub-board 0
almostWinSubBoard :: GameState
almostWinSubBoard = 
    ([ Incomplete [Full X, Full X, Emp, Emp, Emp, Emp, Emp, Emp, Emp]
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     ], X, Nothing)

-- Complex mid-game position
midGame :: GameState
midGame = 
    ([ Incomplete [Full X, Emp, Full O, Emp, Full X, Emp, Emp, Emp, Emp]
     , Incomplete [Emp, Full X, Emp, Full O, Emp, Emp, Emp, Emp, Emp]
     , Incomplete [Full O, Emp, Emp, Emp, Emp, Emp, Emp, Emp, Emp]
     , Incomplete (replicate 9 Emp)
     , Complete (Won X)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     , Incomplete (replicate 9 Emp)
     ], O, Just 1)

-- Late game with few moves
lateGame :: GameState
lateGame = 
    ([ Complete (Won X)
     , Complete (Won O)
     , Incomplete [Full X, Full O, Full X, Full O, Emp, Emp, Emp, Emp, Emp]
     , Complete (Won X)
     , Complete Tie
     , Incomplete [Full O, Full X, Full O, Full X, Emp, Emp, Emp, Emp, Emp]
     , Complete (Won O)
     , Incomplete (replicate 9 Emp)
     , Complete (Won X)
     ], X, Nothing)

-- Test runner
main :: IO ()
main = do
    putStrLn "=== Ultimate Tic-Tac-Toe Performance Tests ==="
    putStrLn ""
    
    -- Test 1: Empty board
    putStrLn "Test 1: Empty Board"
    putStrLn $ "  Legal moves: " ++ show (length $ checkLegalMoves emptyGame)
    timeIt "  Depth 1" $ return $ whoMightWin emptyGame 1 0
    timeIt "  Depth 2" $ return $ whoMightWin emptyGame 2 0
    timeIt "  Depth 3" $ return $ whoMightWin emptyGame 3 0
    putStrLn ""
    
    -- Test 2: Almost winning position (should find winning move fast)
    putStrLn "Test 2: Almost Winning Position (should short-circuit)"
    putStrLn $ "  Legal moves: " ++ show (length $ checkLegalMoves almostWinSubBoard)
    result1 <- timeIt "  Depth 1" $ return $ whoMightWin almostWinSubBoard 1 0
    putStrLn $ "    Result: " ++ show result1
    result2 <- timeIt "  Depth 2" $ return $ whoMightWin almostWinSubBoard 2 0
    putStrLn $ "    Result: " ++ show result2
    result3 <- timeIt "  Depth 3" $ return $ whoMightWin almostWinSubBoard 3 0
    putStrLn $ "    Result: " ++ show result3
    putStrLn ""
    
    -- Test 3: Mid-game position
    putStrLn "Test 3: Mid-Game Position"
    putStrLn $ "  Legal moves: " ++ show (length $ checkLegalMoves midGame)
    timeIt "  Depth 1" $ return $ whoMightWin midGame 1 0
    timeIt "  Depth 2" $ return $ whoMightWin midGame 2 0
    timeIt "  Depth 3" $ return $ whoMightWin midGame 3 0
    putStrLn ""
    
    -- Test 4: Late game
    putStrLn "Test 4: Late Game Position"
    putStrLn $ "  Legal moves: " ++ show (length $ checkLegalMoves lateGame)
    timeIt "  Depth 1" $ return $ whoMightWin lateGame 1 0
    timeIt "  Depth 2" $ return $ whoMightWin lateGame 2 0
    timeIt "  Depth 3" $ return $ whoMightWin lateGame 3 0
    timeIt "  Depth 4" $ return $ whoMightWin lateGame 4 0
    putStrLn ""
    
    -- Test 5: Basic game mechanics (stay in same subboard until complete)
    putStrLn "Test 5: Basic Game Mechanics"
    let state1 = makeMove emptyGame (0, 0)
    putStrLn $ "  After (0,0): forced = " ++ show (getForced state1) ++ " (expected: Just 0)"
    let state2 = makeMove state1 (0, 5)
    putStrLn $ "  After (0,5): forced = " ++ show (getForced state2) ++ " (expected: Just 0)"
    let state3 = makeMove state2 (0, 1)
    putStrLn $ "  After (0,1): forced = " ++ show (getForced state3) ++ " (expected: Just 0)"
    -- Complete sub-board 0, should be free to play anywhere
    let winState = makeMove almostWinSubBoard (0, 2)  -- Completes board 0
    putStrLn $ "  After completing board 0: forced = " ++ show (getForced winState) ++ " (expected: Nothing)"
    putStrLn ""
    
    -- Test 6: Winning detection
    putStrLn "Test 6: Sub-board Win Detection"
    let winState = makeMove almostWinSubBoard (0, 2)
    case getBoard winState !! 0 of
        Complete w -> putStrLn $ "  Sub-board 0 won by: " ++ show w
        _ -> putStrLn "  ERROR: Should have won sub-board 0"
    putStrLn ""
    
    -- Test 7: Rating function sanity check
    putStrLn "Test 7: Rating Function"
    putStrLn $ "  Empty board rating: " ++ show (rateGame emptyGame)
    putStrLn $ "  Almost win rating: " ++ show (rateGame almostWinSubBoard)
    putStrLn $ "  Mid-game rating: " ++ show (rateGame midGame)
    putStrLn ""
    
    -- Test 8: Edge cases - illegal moves
    putStrLn "Test 8: Illegal Move Handling"
    let state1 = makeMove emptyGame (0, 0)
    let state2 = makeMove state1 (0, 0)  -- Try same spot
    let (b1, p1, _) = state1
    let (b2, p2, _) = state2
    putStrLn $ "  Repeat move rejected: " ++ show (b1 == b2 && p1 == p2) ++ " (expected: True)"
    let state3 = makeMove state1 (5, 0)  -- Wrong sub-board (forced to board 0)
    let (b3, p3, _) = state3
    putStrLn $ "  Wrong sub-board rejected: " ++ show (b1 == b3 && p1 == p3) ++ " (expected: True)"
    let state4 = makeMove emptyGame (10, 0)  -- Out of range
    let (b4, p4, _) = emptyGame
    let (b5, p5, _) = state4
    putStrLn $ "  Out of range rejected: " ++ show (b4 == b5 && p4 == p5) ++ " (expected: True)"
    putStrLn ""
    
    -- Test 9: Complete game detection
    putStrLn "Test 9: Game Over Detection"
    let wonGame = ([ Complete (Won X), Complete (Won X), Complete (Won X)
                   , Incomplete (replicate 9 Emp)
                   , Incomplete (replicate 9 Emp)
                   , Incomplete (replicate 9 Emp)
                   , Incomplete (replicate 9 Emp)
                   , Incomplete (replicate 9 Emp)
                   , Incomplete (replicate 9 Emp)
                   ], O, Nothing)
    case checkWinner wonGame of
        Just (Won X) -> putStrLn "  X wins detected: PASS"
        _ -> putStrLn "  X wins detected: FAIL"
    let attemptMove = makeMove wonGame (3, 0)
    putStrLn $ "  No moves after win: " ++ show (wonGame == attemptMove)
    putStrLn ""
    
    -- Test 10: Short-circuit verification (Story 19)
    putStrLn "Test 10: Short-Circuit Optimization (Story 19)"
    putStrLn "  Testing position where first move wins a sub-board..."
    let obviousWin = ([ Incomplete [Full X, Full X, Emp, Emp, Emp, Emp, Emp, Emp, Emp]
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      ], X, Nothing)
    result <- timeIt "    Depth 3 (should be very fast)" $ return $ whoMightWin obviousWin 3 0
    case result of
        (score, move) | score > 0 -> putStrLn $ "    Found good move: " ++ show move ++ " with score " ++ show score ++ " PASS"
        _ -> putStrLn "    WARNING: Should have found positive-scoring move"
    
    -- Test actual game-winning position for short-circuit
    putStrLn "  Testing position where first move wins the GAME..."
    -- X has boards 0,1 won, board 2 has two X's in a row, one move wins game
    let gameWinPos = ([ Complete (Won X), Complete (Won X)
                      , Incomplete [Full X, Full X, Emp, Emp, Emp, Emp, Emp, Emp, Emp]
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                      ], X, Just 2)  -- Forced to board 2, can win immediately
    result2 <- timeIt "    Game win depth 3 (immediate)" $ return $ whoMightWin gameWinPos 3 0
    case result2 of
        (score, move) | score >= 100 -> putStrLn $ "    Found GAME-winning move: " ++ show move ++ " PASS"
        (score, move) -> putStrLn $ "    Got score " ++ show score ++ " (expected 110 for immediate win)"
    putStrLn ""
    
    -- Test 11: Forced board logic (stay in subboard until complete)
    putStrLn "Test 11: Forced Board Mechanics (Stay in subboard)"
    let s1 = makeMove emptyGame (4, 3)  -- Play center board, cell 3
    putStrLn $ "  After (4,3), forced to board: " ++ show (getForced s1) ++ " (expected: Just 4)"
    let s2 = makeMove s1 (4, 8)  -- Stay in board 4, cell 8
    putStrLn $ "  After (4,8), forced to board: " ++ show (getForced s2) ++ " (expected: Just 4)"
    let s3 = makeMove s2 (4, 0)  -- Stay in board 4
    putStrLn $ "  After (4,0), forced to board: " ++ show (getForced s3) ++ " (expected: Just 4)"
    -- Try to play elsewhere (should be rejected)
    let s4 = makeMove s1 (5, 0)  -- Try different board
    let (b1,p1,_) = s1
    let (b4,p4,_) = s4
    putStrLn $ "  Try leaving board 4: " ++ show (b1 == b4 && p1 == p4) ++ " (expected: True - rejected)"
    -- Complete a board
    let almostComplete = ([ Incomplete [Full X, Full X, Emp,  -- Row 0: need position 2
                                       Full O, Full O, Emp,  -- Row 1
                                       Emp, Emp, Emp]        -- Row 2
                          , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                          , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                          , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                          , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                          ], X, Just 0)
    let s5 = makeMove almostComplete (0, 2)  -- Complete row 0, win board 0
    putStrLn $ "  After completing board: forced = " ++ show (getForced s5) ++ " (expected: Nothing)"
    putStrLn ""
    
    -- Test 12: Performance regression check
    putStrLn "Test 12: Performance Regression Check"
    putStrLn "  These should all complete in under 5 seconds:"
    timeIt "    Empty board depth 3" $ return $ whoMightWin emptyGame 3 0
    timeIt "    Mid-game depth 4" $ return $ whoMightWin midGame 4 0
    timeIt "    Late game depth 5" $ return $ whoMightWin lateGame 5 0
    putStrLn ""
    
    -- Test 13: Move count sanity
    putStrLn "Test 13: Legal Move Counting"
    let moveCount1 = length $ checkLegalMoves emptyGame
    putStrLn $ "  Empty board: " ++ show moveCount1 ++ " moves (expected: 81)"
    let oneMove = makeMove emptyGame (0, 0)
    let moveCount2 = length $ checkLegalMoves oneMove
    putStrLn $ "  After one move (forced): " ++ show moveCount2 ++ " moves (expected: 8)"
    putStrLn ""
    
    -- Test 14: Rating consistency
    putStrLn "Test 14: Rating Function Consistency"
    let xWinning = ([ Complete (Won X), Complete (Won X), Incomplete (replicate 9 Emp)
                    , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                    , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                    , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                    ], X, Nothing)
    let rating1 = rateGame xWinning
    putStrLn $ "  X with 2 boards: " ++ show rating1 ++ " (should be positive)"
    let oWinning = ([ Complete (Won O), Complete (Won O), Incomplete (replicate 9 Emp)
                    , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                    , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                    , Incomplete (replicate 9 Emp), Incomplete (replicate 9 Emp)
                    ], X, Nothing)
    let rating2 = rateGame oWinning
    putStrLn $ "  O with 2 boards (X's turn): " ++ show rating2 ++ " (should be negative)"
    putStrLn ""
    
    putStrLn "=== All Tests Complete ==="

-- Helper functions
getForced :: GameState -> Maybe Location
getForced (_, _, f) = f

getBoard :: GameState -> Board
getBoard (b, _, _) = b

--Used AI to generate these test cases