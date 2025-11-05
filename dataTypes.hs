data Spot = Player Symbol | Empty deriving (Show, Eq) -- maybe ord?
data Board = Incomplete [Spot] | Complete Spot deriving (Show, Eq)
data Symbol = X | O deriving (Show, Eq)

-- obviously this is just some of my code, you guys feel free to choose what you want from our doc
-- i just put mine in as a placeholder