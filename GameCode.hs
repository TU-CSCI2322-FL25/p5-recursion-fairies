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
    auxMain boardLoc [] = []
    auxMain boardLoc ((Incomplete sub):xs) =
        auxSub boardLoc 0 sub ++ auxMain (boardLoc + 1) xs
    auxMain boardLoc ((Complete _):xs) =
        auxMain (boardLoc + 1) xs

    auxSub boardLoc subBoardLoc [] = []
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
  in checkSubBoard newSpots
placeSpot _ board@(Complete _) _ = board  -- no change if already complete

-- Check if a sub-board has a winner or tie
checkSubBoard :: [Spot] -> SubBoard
checkSubBoard spots
  | any (allOwnedBy X) lines = Complete $ Won X
  | any (allOwnedBy O) lines = Complete $ Won O
  | all isFull spots         = Complete Tie
  | otherwise                = Incomplete spots
  where
    lines = [[0,1,2],[3,4,5],[6,7,8],
             [0,3,6],[1,4,7],[2,5,8],
             [0,4,8],[2,4,6]]
    allOwnedBy p idxs = all (== Full p) [spots !! i | i <- idxs]
    isFull (Full _)   = True
    isFull Emp        = False


-- Replace one sub-board inside the full board
updateBoard :: Board -> Location -> SubBoard -> Board
updateBoard board idx newSub = take idx board ++ [newSub] ++ drop (idx + 1) board


-- Check overall game winner 
gameWinner :: Board -> Maybe Winner
gameWinner board
  | any (all (owned X)) lines = Just $ Won X
  | any (all (owned O)) lines = Just $ Won O
  | all isComplete board = Just Tie
  | otherwise = Nothing
  where
    owned p i  = case board !! i of
      Complete (Won pl) -> p == pl
      _ -> False
    lines = [[0,1,2],[3,4,5],[6,7,8],
             [0,3,6],[1,4,7],[2,5,8],
             [0,4,8],[2,4,6]]
    isComplete (Complete _) = True
    isComplete (Incomplete _) = False

                

-- Core function: make a legal move
makeMove :: GameState -> Move -> GameState
makeMove (board, player, forced) (subLoc, cellLoc)
  | isJust (gameWinner board) = (board, player, Nothing)
  | subLoc < 0 || subLoc > 8 = (board, player, forced) -- Out of bounds
  | cellLoc < 0 || cellLoc > 8 = (board, player, forced)
  | Just f <- forced, f /= subLoc = (board, player, forced) -- Forced board rule violated
  | Complete _ <- board !! subLoc = (board, player, forced) -- Can't play in completed sub-board
  | Incomplete spots <- board !! subLoc, Full _ <- spots !! cellLoc = (board, player, forced) -- Can't play in a full cell

makeMove (board, player, _) (subLoc, cellLoc)
  | Incomplete spots <- board !! subLoc, Emp <- spots !! cellLoc =
      let newSub   = placeSpot player (Incomplete spots) cellLoc
          newBoard = updateBoard board subLoc newSub
          overall  = gameWinner newBoard

          nextForced = -- You must go to the sub-board indicated by the cell you just played.
            case newBoard !! cellLoc of
              Incomplete _ -> Just cellLoc
              Complete _   -> Nothing

      in case overall of
           Nothing -> (newBoard, nextPlayer player, nextForced)
           Just _ -> (newBoard, player, Nothing)

whoWillWin :: GameState -> Winner
whoWillWin state =
  bfs [state]
  where
    legalMoves = checkLegalMoves state

    bfs [] = error "No winner found"
    bfs layer =
      case findWinner layer of
        Just w -> w
        Nothing -> bfs (concat [map (makeMove s) legalMoves | s <- layer])
      
    findWinner [] = Nothing
    findWinner ((board, _, _):rest) =
      case gameWinner board of
        Just w -> Just w
        Nothing -> findWinner rest

-- bestOutcome :: Player -> [Winner] -> Winner
-- bestOutcome X outcomes
--   | Won X `elem` outcomes = Won X
--   | Tie  `elem` outcomes  = Tie
--   | otherwise             = Won O
-- bestOutcome O outcomes
--   | Won O `elem` outcomes = Won O
--   | Tie  `elem` outcomes  = Tie
--   | otherwise             = Won X
