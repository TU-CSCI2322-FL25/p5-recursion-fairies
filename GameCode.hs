module GameCode where
-- data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
-- data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
-- data Symbol = X | O deriving (Show, Eq)

import Data.Maybe

--import all other files that are needed
import DataTypes
import GameText

-- GAME FUNCS
checkLegalMoves :: GameState -> [Move]
checkLegalMoves (board, player, Nothing) =
  let
    auxMain boardLoc [] = [] -- auxMain is a recursive function that goes through a board and calls auxSub on all incomplete boards (boards with legal moves).
    auxMain boardLoc ((Incomplete sub):xs) =
        auxSub boardLoc 0 sub ++ auxMain (boardLoc + 1) xs
    auxMain boardLoc ((Complete _):xs) =
        auxMain (boardLoc + 1) xs

    auxSub boardLoc subBoardLoc [] = []-- auxSub is a recursive function that goes through a subboard and finds all the legal moves in the board, returning legal moves as a Move.
    auxSub boardLoc subBoardLoc (Emp:xs) =
        (boardLoc, subBoardLoc):auxSub boardLoc (subBoardLoc + 1) xs
    auxSub boardLoc subBoardLoc ((Full _):xs) =
        auxSub boardLoc (subBoardLoc + 1) xs
  in auxMain 0 board
checkLegalMoves (board, player, Just forced) = case board !! forced of
  Complete _ -> []
  Incomplete sub ->
    let
      auxSub _ [] = []
      auxSub subLoc (Emp:xs) = (forced, subLoc):auxSub (subLoc + 1) xs
      auxSub subLoc ((Full _):xs) = auxSub (subLoc + 1) xs
    in auxSub 0 sub

-- Place a player's mark in a given spot of a sub-board
placeSpot :: Player -> SubBoard -> Location -> SubBoard
placeSpot p (Incomplete spots) loc =
  let newSpots = take loc spots ++ [Full p] ++ drop (loc + 1) spots
  in updateSubBoard newSpots
placeSpot _ board@(Complete _) _ = board  -- no change if already complete

-- Check if a sub-board has a winner or tie
updateSubBoard :: [Spot] -> SubBoard
updateSubBoard spots
  | any (allOwnedBy X) (lines spots) = Complete $ Won X
  | any (allOwnedBy O) (lines spots) = Complete $ Won O
  | all isFull spots         = Complete Tie
  | otherwise                = Incomplete spots
  where
    lines [s0,s1,s2,s3,s4,s5,s6,s7,s8] = 
        [[s0,s1,s2],[s3,s4,s5],[s6,s7,s8], 
        [s0,s3,s6],[s1,s4,s7],[s2,s5,s8], 
        [s0,s4,s8],[s2,s4,s6]] 
    lines _ = error "invalid board"
    allOwnedBy p = all (== Full p)
    isFull (Full _)   = True
    isFull Emp        = False

winnerOfBoard :: Board -> Maybe Winner
winnerOfBoard board
  | any (allOwnedBy X) (lines board) = Just $ Won X
  | any (allOwnedBy O) (lines board) = Just $ Won O
  | all isComplete board = Just Tie
  | otherwise = Nothing
  where
    allOwnedBy p = all (== Complete (Won p))
    
    lines [s0,s1,s2,s3,s4,s5,s6,s7,s8] = 
        [[s0,s1,s2],[s3,s4,s5],[s6,s7,s8], 
        [s0,s3,s6],[s1,s4,s7],[s2,s5,s8], 
        [s0,s4,s8],[s2,s4,s6]] 
    lines _ = error "invalid board"
    isComplete (Complete _) = True
    isComplete _ = False

checkWinner :: GameState -> Maybe Winner
checkWinner (board, _, _) = winnerOfBoard board

--Core function: make a legal move
--Helpers
inRange :: Int -> Bool
inRange n = n >= 0 && n <= 8

obeysForce :: Maybe Location -> Location -> Bool
obeysForce Nothing  _ = True
obeysForce (Just f) l = f == l


makeMove :: GameState -> Move -> GameState
makeMove state@(board, player, forced) (sbLoc, cellLoc)
  | isJust (checkWinner state)        = (board, player, Nothing)
  | not (inRange sbLoc)              = (board, player, Nothing)
  | not (inRange cellLoc)            = (board, player, Nothing)
  | not (obeysForce forced sbLoc)    = (board, player, Nothing)
  | otherwise =
      case splitAt sbLoc board of
        (_, []) -> (board, player, Nothing)
        (boardsBefore, sub : boardsAfter) -> case sub of
          Complete _ -> (board, player, Nothing)
          Incomplete spots -> case splitAt cellLoc spots of
              (_, []) -> (board, player, Nothing)

              (cellsBefore, cell : cellsAfter) -> case cell of
                Full _ -> (board, player, Nothing)
                Emp ->
                  let
                    -- update sub-board
                    newCells   = cellsBefore ++ Full player : cellsAfter
                    newSub     = updateSubBoard newCells
                    newBoard   = boardsBefore ++ newSub : boardsAfter
                    endingState   = winnerOfBoard newBoard

                    nextForced =
                      case splitAt sbLoc newBoard of
                        (_, target : _) -> case target of
                          Incomplete _ -> Just sbLoc
                          Complete _   -> Nothing
                        _ -> Nothing
                  in
                    case endingState of
                      Nothing -> (newBoard, nextPlayer player, nextForced)
                      Just _  -> (newBoard, player, Nothing)