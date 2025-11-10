-- data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
-- data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
-- data Symbol = X | O deriving (Show, Eq)
import Data.List (intercalate)
import Data.List.Split (chunksOf)

type Location = Int
allLocations = [0..8]
data Spot = Full Player | Emp deriving (Show, Eq)
data SubBoard = Incomplete [Spot] | Complete Winner deriving (Show, Eq)
type Board = [SubBoard]
data Player = X | O deriving (Show, Eq)
data Winner = Won Player | Tie | Unfinished deriving (Show, Eq)
type GameState = (Board, Player, Maybe Location)
type Move = (Location, Location)


-- nextPlayer :: player -> player
-- nextPlayer X = O
-- nextPlayer O = X


-- gameWinner :: Board -> Player
--Checks the Board, returns Empty if game is a draw, Cont if Game is still continuing, X or O for if respective player won
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- prettyPrint :: GameState -> String
    
-- tester board
-- subBoard1 = Incomplete [Full X, Full O, Emp, Full X, Full O, Emp, Full O, Full X, Emp]
-- subBoard2 = Complete $ Won X
-- board = [subBoard1, subBoard1, subBoard 2, subBoard1, subBoard 1, subBoard 2, subBoard 2, subBoard 2, subBoard 1]
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

-- prettyPrint :: GameState -> String
    
-- tester board
-- subBoard1 = Incomplete [Full X, Full O, Emp, Full X, Full O, Emp, Full O, Full X, Emp]
-- subBoard2 = Complete $ Won X
-- board = [subBoard1, subBoard1, subBoard 2, subBoard1, subBoard 1, subBoard 2, subBoard 2, subBoard 2, subBoard 1]
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

-- gameWinner :: Board -> Player
--Checks the Board, returns Empty if game is a draw, Cont if Game is still continuing, X or O for if respective player won

-- i am going insane please give me a for loop haskell
checkLegalMoves :: Board -> [Move]
checkLegalMoves game = let -- auxMain is a recursive function that goes through a board and calls auxSub on all incomplete boards (boards with legal moves).
                           auxMain _ [] = []
                           auxMain boardLoc ((Incomplete sub):xs) = auxSub boardLoc 0 sub ++ auxMain (boardLoc + 1) xs
                           auxMain boardLoc ((Complete _):xs) = auxMain (boardLoc + 1) xs
                           -- auxSub is a recursive function that goes through a subboard and finds all the legal moves in the board, returning legal moves as a Move.
                           auxSub _ _ [] = []
                           auxSub boardLoc subBoardLoc (Emp:xs) = (boardLoc, subBoardLoc):auxSub boardLoc (subBoardLoc + 1) xs
                           auxSub boardLoc subBoardLoc ((Full _):xs) = auxSub boardLoc (subBoardLoc + 1) xs
                       in  auxMain 0 game

-- obviously this is just some of my code, you guys feel free to choose what you want from our doc
-- i just put mine in as a placeholder