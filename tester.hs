module Tests where

import Test.HUnit
import Main
-- Assumes your main game code is in Main.hs
-- If not, change the import to match your module name.

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

emptySub :: SubBoard
emptySub = Incomplete (replicate 9 Emp)

emptyBoard :: Board
emptyBoard = replicate 9 emptySub

startState :: GameState
startState = (emptyBoard, X, Nothing)

---------------------------------------------------------------------
-- TEST 1: Basic legal move
---------------------------------------------------------------------

test_basicMove :: Test
test_basicMove = TestCase $ do
    let (b, p, f) = makeMove startState (0,0)
    -- cell 0 in sub-board 0 becomes X
    case b !! 0 of
      Incomplete spots -> spots !! 0 @?= Full X
      _ -> assertFailure "Sub-board 0 should be Incomplete after first move."

    p @?= O
    f @?= Just 0

---------------------------------------------------------------------
-- TEST 2: Illegal move due to wrong forced board
---------------------------------------------------------------------

test_wrongForcedBoard :: Test
test_wrongForcedBoard = TestCase $ do
    let s1 = makeMove startState (0,0)
        s2 = makeMove s1 (1,0)  -- illegal (forced = 0)
    s2 @?= s1  -- must be unchanged

---------------------------------------------------------------------
-- TEST 3: Legal move in forced board
---------------------------------------------------------------------

test_legalForcedBoard :: Test
test_legalForcedBoard = TestCase $ do
    let s1 = makeMove startState (0,0)
        s2@(b,p,f) = makeMove s1 (0,1)
    case b !! 0 of
      Incomplete spots -> spots !! 1 @?= Full O
      _ -> assertFailure "Sub-board 0 should remain Incomplete."
    p @?= X
    f @?= Just 1

---------------------------------------------------------------------
-- TEST 4: Illegal move on filled cell
---------------------------------------------------------------------

test_filledCell :: Test
test_filledCell = TestCase $ do
    let s1 = makeMove startState (0,0)
        s2 = makeMove s1 (0,1)
        s3 = makeMove s2 (1,1)  -- O → forced to 1
        s4 = makeMove s3 (1,1)  -- illegal (already filled by O)
    s4 @?= s3

---------------------------------------------------------------------
-- TEST 5: Winning a sub-board
---------------------------------------------------------------------

test_subBoardWin :: Test
test_subBoardWin = TestCase $ do
    let sb = Incomplete [Full X, Full X, Emp,
                         Emp, Emp, Emp,
                         Emp, Emp, Emp]
        board = sb : replicate 8 emptySub
        s0 = (board, X, Nothing)
        (b,p,f) = makeMove s0 (0,2)   -- X wins sub-board 0

    b !! 0 @?= Complete (Won X)
    p @?= O
    f @?= Just 2

---------------------------------------------------------------------
-- TEST 6: Forced board becomes Nothing when target is complete
---------------------------------------------------------------------

test_forcedGoesNothing :: Test
test_forcedGoesNothing = TestCase $ do
    let preBoard = Complete (Won X) : replicate 8 emptySub
        s0 = (preBoard, X, Nothing)
        (_,_,f) = makeMove s0 (2,5)   -- next forced = sub-board 5
    -- but sub-board 5 is empty — wait:
    -- we need sub-board 5 to be Complete for this test
    -- Let's actually setup properly:
    let fixedBoard = [emptySub, emptySub, emptySub,
                      emptySub, emptySub, Complete (Won X),
                      emptySub, emptySub, emptySub]
        s1 = (fixedBoard, X, Nothing)
        (_,_,forced) = makeMove s1 (4,5)
    forced @?= Nothing

---------------------------------------------------------------------
-- TEST 7: Entire game is won
---------------------------------------------------------------------

test_gameWin :: Test
test_gameWin = TestCase $ do
    let bw = [ Complete (Won X), Complete (Won X), Complete (Won X)
             , emptySub, emptySub, emptySub
             , emptySub, emptySub, emptySub ]
        s0 = (bw, O, Nothing)
        (b,p,f) = makeMove s0 (3,0)

    b @?= bw             -- board doesn't change the winner logic
    p @?= O              -- player does NOT change when game is finished
    f @?= Nothing        
    gameWinner b @?= Won X

---------------------------------------------------------------------
-- TEST 8: Tie board
---------------------------------------------------------------------

test_tieBoard :: Test
test_tieBoard = TestCase $ do
    let tb = replicate 9 (Complete Tie)
    gameWinner tb @?= Tie

---------------------------------------------------------------------
-- TEST 9: Legal move enumeration
---------------------------------------------------------------------

test_legalMoves :: Test
test_legalMoves = TestCase $ do
    length (checkLegalMoves emptyBoard) @?= 81

---------------------------------------------------------------------
-- Run all tests
---------------------------------------------------------------------

tests :: Test
tests = TestList
  [ TestLabel "Basic move"               test_basicMove
  , TestLabel "Wrong forced board"       test_wrongForcedBoard
  , TestLabel "Legal forced board"       test_legalForcedBoard
  , TestLabel "Filled cell illegal"      test_filledCell
  , TestLabel "Sub-board win"            test_subBoardWin
  , TestLabel "Forced board → Nothing"   test_forcedGoesNothing
  , TestLabel "Game winner"              test_gameWin
  , TestLabel "Tie board"                test_tieBoard
  , TestLabel "Legal move count"         test_legalMoves
  ]

runTests :: IO ()
runTests = runTestTT tests >> pure ()

-- Used ChatGPT to set up test cases