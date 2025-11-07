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
data GameState = Continuing Board Player | Finished Winner deriving (Show, Eq)
type Move = (Location, Location)


nextPlayer :: player -> player
nextPlayer X = O
nextPlayer O = X


gameWinner :: Board -> Player
--Checks the Board, returns Empty if game is a draw, Cont if Game is still continuing, X or O for if respective player won
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

prettyPrint :: Game -> String


-- obviously this is just some of my code, you guys feel free to choose what you want from our doc
-- i just put mine in as a placeholder