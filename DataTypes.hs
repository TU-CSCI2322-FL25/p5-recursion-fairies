module DataTypes where
    
    
    type Location = Int
    allLocations = [0..8] :: [Location]

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