module Tests where

import GameCode

------------------------------------------------------------
-- Simple test harness
------------------------------------------------------------

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert name expected actual =
    if expected == actual
      then putStrLn ("PASS: " ++ name)
      else do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  Expected: " ++ show expected)
        putStrLn ("  Actual:   " ++ show actual)

assertBool :: String -> Bool -> IO ()
assertBool name cond =
    if cond
      then putStrLn ("PASS: " ++ name)
      else putStrLn ("FAIL: " ++ name)

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

emptySub :: SubBoard
emptySub = Incomplete (replicate 9 Emp)

emptyBoard :: Board
emptyBoard = replicate 9 emptySub

startState :: GameState
startState = (emptyBoard, X, Nothing)

------------------------------------------------------------
-- TEST 1: Basic legal move
------------------------------------------------------------

test_basicMove :: IO ()
test_basicMove = do
    let (b,p,f) = makeMove startState (0,0)
    case b !! 0 of
      Incomplete spots -> assert "BasicMove: spot" (Full X) (spots !! 0)
      _ -> putStrLn "FAIL: BasicMove – sub-board should be Incomplete"
    assert "BasicMove: nextPlayer" O p
    assert "BasicMove: forced" (Just 0) f

------------------------------------------------------------
-- TEST 2: Illegal move due to wrong forced board
------------------------------------------------------------

test_wrongForcedBoard :: IO ()
test_wrongForcedBoard = do
    let s1 = makeMove startState (0,0)
        s2 = makeMove s1 (1,0) -- wrong forced board
    assert "WrongForcedBoard" s1 s2

------------------------------------------------------------
-- TEST 3: Legal forced-board move
------------------------------------------------------------

test_legalForcedBoard :: IO ()
test_legalForcedBoard = do
    let s1 = makeMove startState (0,0)
        (b,p,f) = makeMove s1 (0,1)
    case b !! 0 of
      Incomplete spots -> assert "LegalForced: placed O" (Full O) (spots !! 1)
      _ -> putStrLn "FAIL: LegalForced – board complete unexpectedly"
    assert "LegalForced: next player" X p
    assert "LegalForced: next forced" (Just 1) f

------------------------------------------------------------
-- TEST 4: Illegal move on filled cell
------------------------------------------------------------

test_filledCell :: IO ()
test_filledCell = do
    let s1 = makeMove startState (0,0)
        s2 = makeMove s1 (0,1)
        s3 = makeMove s2 (1,1)
        s4 = makeMove s3 (1,1) -- illegal
    assert "FilledCellIllegal" s3 s4

------------------------------------------------------------
-- TEST 5: Winning a sub-board
------------------------------------------------------------

test_subBoardWin :: IO ()
test_subBoardWin = do
    let sb = Incomplete
              [ Full X, Full X, Emp
              , Emp, Emp, Emp
              , Emp, Emp, Emp ]

        board = sb : replicate 8 emptySub
        s0 = (board, X, Nothing)

        (b,p,f) = makeMove s0 (0,2)

    assert "SubBoardWin: board winner" (Complete (Won X)) (b !! 0)
    assert "SubBoardWin: next player" O p
    assert "SubBoardWin: forced" (Just 2) f

------------------------------------------------------------
-- TEST 6: Forced board becomes Nothing when target is complete
------------------------------------------------------------

test_forcedGoesNothing :: IO ()
test_forcedGoesNothing = do
    let testBoard =
          [ emptySub
          , emptySub
          , emptySub
          , emptySub
          , emptySub
          , Complete (Won X)
          , emptySub
          , emptySub
          , emptySub ]

        s0 = (testBoard, X, Nothing)
        (_,_,f) = makeMove s0 (4,5)

    assert "Forced → Nothing" Nothing f

------------------------------------------------------------
-- TEST 7: Entire game is already won
------------------------------------------------------------

test_gameWin :: IO ()
test_gameWin = do
    let bw =
          [ Complete (Won X), Complete (Won X), Complete (Won X)
          , emptySub, emptySub, emptySub
          , emptySub, emptySub, emptySub ]

        s0 = (bw, O, Nothing)
        (b,p,f) = makeMove s0 (3,0)

    assert "GameWin: board unchanged" bw b
    assert "GameWin: player unchanged" O p
    assert "GameWin: forced unchanged" Nothing f
    assert "GameWin: winner" (Won X) (gameWinner b)

------------------------------------------------------------
-- TEST 8: Full tie board
------------------------------------------------------------

test_tieBoard :: IO ()
test_tieBoard = do
    let tb = replicate 9 (Complete Tie)
    assert "TieBoardWinner" Tie (gameWinner tb)

------------------------------------------------------------
-- TEST 9: Legal move enumeration
------------------------------------------------------------

test_legalMoves :: IO ()
test_legalMoves = do
    assert "LegalMoves: count" 81 (length (checkLegalMoves emptyBoard))

------------------------------------------------------------
-- Full Test Suite Runner
------------------------------------------------------------

runTests :: IO ()
runTests = do
    putStrLn "Running tests..."
    test_basicMove
    test_wrongForcedBoard
    test_legalForcedBoard
    test_filledCell
    test_subBoardWin
    test_forcedGoesNothing
    test_gameWin
    test_tieBoard
    test_legalMoves
    putStrLn "All tests done."

--Used ChatGPT to set up testing