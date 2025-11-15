module GameCode where
-- data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
-- data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
-- data Symbol = X | O deriving (Show, Eq)

import Data.Maybe

--import all other files that are needed
import DataTypes
import GamePrint
import InputText

-- GAME FUNCS
checkWinner :: [Location] -> Bool
checkWinner xs = 
    let b1 = checkVertical 0 xs   || checkVertical 1 xs   || checkVertical 2 xs
        b2 = checkHorizontal 0 xs || checkHorizontal 3 xs || checkHorizontal 6 xs
        b3 = checkDiagonalLeft xs || checkDiagonalRight xs
    in b1 || b2 || b3

checkVertical :: Location -> [Location] -> Bool
checkVertical a xs = (a `elem` xs) && ((a+3) `elem` xs) && ((a+6) `elem` xs )
checkHorizontal :: Location -> [Location] -> Bool
checkHorizontal a xs = (a `elem` xs) && ((a+1) `elem` xs) && ((a+2) `elem` xs)
checkDiagonalLeft :: [Location] -> Bool
checkDiagonalLeft xs = (0 `elem` xs) && (4 `elem` xs) && (8 `elem` xs)
checkDiagonalRight :: [Location] -> Bool
checkDiagonalRight xs = (2 `elem` xs) && (4 `elem` xs) && (6 `elem` xs)

checkAllFull :: Board -> Bool
checkAllFull [] = True
checkAllFull ((Complete a):xs) = checkAllFull xs
checkAllFull _ = False


subBoardWinner:: SubBoard -> Winner
subBoardWinner (Complete a) = a
subBoardWinner (Incomplete spots) = 
            let locs = zip allLocations spots
                xsFull = catMaybes $ map isPlayerX locs
                osFull = catMaybes $ map isPlayerO locs
                xWins = checkWinner xsFull
                oWins = checkWinner osFull
                boardFull = length xsFull + length osFull == 9
            in if xWins 
            then Won X
            else if oWins 
                 then Won O 
                 else if boardFull
                      then Tie
                      else Unfinished
gameWinner :: Board -> Winner
gameWinner brd = let temp = (boardToSubBoard brd)
                     fullStatus = (checkAllFull brd)
                     won = subBoardWinner (Incomplete temp)
                 in if won == Unfinished
                    then if fullStatus
                         then Tie
                         else Unfinished
                    else won


boardToSubBoard :: Board -> [Spot]
boardToSubBoard [] = []
boardToSubBoard ((Complete (Won a)):xs) =(Full a):(boardToSubBoard xs)
boardToSubBoard (x:xs) = Emp:(boardToSubBoard xs)

checkLegalMoves :: Board -> [Move]
checkLegalMoves game = let -- auxMain is a recursive function that goes through a board and calls auxSub on all incomplete boards (boards with legal moves).
  auxMain _ [] = []
  auxMain boardLoc ((Incomplete sub):xs) = auxSub boardLoc 0 sub ++ auxMain (boardLoc + 1) xs
  auxMain boardLoc ((Complete _):xs) = auxMain (boardLoc + 1) xs
  -- auxSub is a recursive function that goes through a subboard and finds all the legal moves in the board, returning legal moves as a Move.
  auxSub _ _ [] = []
  auxSub boardLoc subBoardLoc (Emp:xs) = (boardLoc, subBoardLoc):auxSub boardLoc (subBoardLoc + 1) xs
  auxSub boardLoc subBoardLoc ((Full _):xs) = auxSub boardLoc (subBoardLoc + 1) xs
  in auxMain 0 game

-- Place a player's mark in a given spot of a sub-board
placeSpot :: Player -> SubBoard -> Location -> SubBoard
placeSpot p (Incomplete spots) loc =
  let newSpots = take loc spots ++ [Full p] ++ drop (loc + 1) spots
      winner   = checkSubBoard newSpots
  in case winner of
       Unfinished -> Incomplete newSpots
       _          -> Complete winner
placeSpot _ board@(Complete _) _ = board  -- no change if already complete

-- Check if a sub-board has a winner or tie
checkSubBoard :: [Spot] -> Winner
checkSubBoard spots
  | any (allOwnedBy X) lines = Won X
  | any (allOwnedBy O) lines = Won O
  | all isFull spots         = Tie
  | otherwise                = Unfinished
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
checkOverall :: Board -> Winner
checkOverall board =
  let subResults = map subToWinner board
      owned p i  = subResults !! i == Won p
      lines = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]
  in if any (all (owned X)) lines then Won X
     else if any (all (owned O)) lines then Won O
     else if all (\w -> w /= Unfinished) subResults then Tie
     else Unfinished
  where
    subToWinner (Complete w) = w
    subToWinner _            = Unfinished

                

-- Core function: make a legal move
makeMove :: GameState -> Move -> GameState
makeMove (board, player, forced) (subLoc, cellLoc)
  | gameWinner board /= Unfinished = (board, player, Nothing)
  | subLoc < 0 || subLoc > 8 = (board, player, forced) -- Out of bounds
  | cellLoc < 0 || cellLoc > 8 = (board, player, forced)
  | Just f <- forced, f /= subLoc = (board, player, forced) -- Forced board rule violated
  | Complete _ <- board !! subLoc = (board, player, forced) -- Can't play in completed sub-board
  | Incomplete spots <- board !! subLoc, Full _ <- spots !! cellLoc = (board, player, forced) -- Can't play in a full cell

makeMove (board, player, _) (subLoc, cellLoc)
  | Incomplete spots <- board !! subLoc, Emp <- spots !! cellLoc =
      let newSub   = placeSpot player (Incomplete spots) cellLoc
          newBoard = updateBoard board subLoc newSub
          overall  = checkOverall newBoard

          nextForced = -- You must go to the sub-board indicated by the cell you just played.
            case newBoard !! cellLoc of
              Incomplete _ -> Just cellLoc
              Complete _   -> Nothing

      in case overall of
           Unfinished -> (newBoard, nextPlayer player, nextForced)
           w          -> (newBoard, player, Nothing)