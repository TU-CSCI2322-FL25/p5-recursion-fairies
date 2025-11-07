-- data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
-- data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
-- data Symbol = X | O deriving (Show, Eq)

type Location = Int
allLocations = [0..8]
Data Spot = X | O | Emp | Cont
type BoardSquare = (Location, Spot)
Data SubBoard = Incomplete [BoardSquare] | Complete Location Spot
type Board = [SubBoard]
type Player = Spot
type Move = (Location, Location, Spot)


nextPlayer :: player -> player
nextPlayer X = O
nextPlayer O = X


gameWinner :: Board -> Player
--Checks the Board, returns Empty if game is a draw, Cont if Game is still continuing, X or O for if respective player won
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- obviously this is just some of my code, you guys feel free to choose what you want from our doc
-- i just put mine in as a placeholder