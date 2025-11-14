-- data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
-- data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
-- data Symbol = X | O deriving (Show, Eq)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Maybe
-- DEFINE ALL DATA TYPES
allLocations = [0..8]
type Location = Int

data Player = X | O deriving (Show, Eq)
data Spot = Full Player | Emp deriving (Show, Eq)
data Winner = Won Player | Tie | Unfinished deriving (Show, Eq)
data SubBoard = Incomplete [Spot] | Complete Winner deriving (Show, Eq)
type Board = [SubBoard]

type Move = (Location, Location)
type GameState = (Board, Player, Maybe Location)

-- DEFINE FUNCTIONS FOR DATA TYPES
isPlayerX :: (Location, Spot) -> Maybe Location
isPlayerX (i, Full X) = Just i
isPlayerX _ = Nothing
isPlayerO :: (Location, Spot) -> Maybe Location
isPlayerO (i, Full O) = Just i
isPlayerO _ = Nothing

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- PRINTING BOARD FUNCS
-- tester board
--subBoard1 = Incomplete [Full X, Full O, Emp, Full X, Full O, Emp, Full O, Full X, Emp]
--subBoard2 = Complete $ Won X
--board = [subBoard1, subBoard1, subBoard2, subBoard1, subBoard1, subBoard2, subBoard2, subBoard2, subBoard1]
subBoardStr :: SubBoard -> (String, String, String)
subBoardStr (Complete w) = 
    ("           ", 
    "     " ++ winState ++ "     ",
    "           ")
  where
    winState = case w of
        Won X       -> "X"
        Won O       -> "O"
        Tie         -> "T"
        Unfinished  -> " "
subBoardStr (Incomplete lst) =
    let rows = chunksOf 3 (map showSpot lst)
        rowStrs = map (intercalate "|") rows
    in
      case rowStrs of
        [r1, r2, r3] -> (r1, r2, r3)

printMainBoard :: Board -> String
printMainBoard board =
  let subStrs = map subBoardStr board
      mainRows = chunksOf 3 subStrs
      totalRow row =
        let (r1s, r2s, r3s) = unzip3 row
        in unlines [intercalate "||" r1s, intercalate "||" r2s, intercalate "||" r3s]
  in intercalate "=====================================" $ map totalRow mainRows



printSubBoard :: SubBoard -> String
printSubBoard (Complete w) = 
    "           \n" ++
    "     " ++ winState ++ "     \n" ++
    "           \n"
  where
    winState = case w of
        Won X       -> "X"
        Won O       -> "O"
        Tie         -> "T"
        Unfinished  -> " "
        
printSubBoard (Incomplete lst) =
    let rows = chunksOf 3 (map showSpot lst)
    in unlines [intercalate "|" r | r <- rows]

showSpot :: Spot -> String
showSpot (Full X) = " X "
showSpot (Full O) = " O "
showSpot Emp      = "   "

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
checkAllFull ((Complete a):xs) = True && (checkAllFull xs)
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