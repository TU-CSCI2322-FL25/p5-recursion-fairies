-- data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
-- data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
-- data Symbol = X | O deriving (Show, Eq)

type Location = Int
allLocations = [0..8]
data Spot = Full Player | Emp deriving (Show, Eq)
data SubBoard = Incomplete [Spot] | Complete Winner deriving (Show, Eq)
type Board = [SubBoard]
data Player = X | O deriving (Show, Eq)
data Winner = Won Player | Tie | Unfinished deriving (Show, Eq)
type GameState = (Board, Player, Maybe Location)
type Move = (Location, Location)


nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X


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