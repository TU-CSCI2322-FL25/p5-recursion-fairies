type Location = Int 
allLocations = [0..8]
data Spot = Full Player | Emp deriving (Show, Eq)
data SubBoard = Incomplete [Spot] | Complete Winner deriving (Show, Eq)
type Board = [SubBoard]
data Player = X | O deriving (Show, Eq)
data Winner = Won Player | Tie | Unfinished deriving (Show, Eq)
data GameState = Continuing Board Player | Finished Winner deriving (Show, Eq)
type Move = (Location, Location)

-- Switch player after a valid move
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X


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
makeMove (Finished w) _ = Finished w  -- game already over
makeMove (Continuing board player) (subLoc, cellLoc) =
  case board !! subLoc of
    Complete _ -> Continuing board player  -- can't move in a finished sub-board
    Incomplete spots ->
      case spots !! cellLoc of
        Full _ -> Continuing board player  -- can't move in occupied cell
        Emp ->
          let newSub   = placeSpot player (Incomplete spots) cellLoc
              newBoard = updateBoard board subLoc newSub
              overall  = checkOverall newBoard
          in case overall of
               Unfinished -> Continuing newBoard (nextPlayer player)
               _          -> Finished overall