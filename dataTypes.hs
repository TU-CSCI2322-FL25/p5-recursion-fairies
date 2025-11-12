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


gameWinner :: Board -> Winner
gameWinner brd = let temp = (boardToSubBoard brd)
                 in subBoardWinner temp
                     

isPlayerX :: (Location, Spot) -> Maybe Location
isPlayerX (i, Full X) = Just i
isPlayerX _ = Nothing

isPlayerO :: (Location, Spot) -> Maybe Location
isPlayerO (i, Full O) = Just i
isPlayerO _ = Nothing

subBoardWinner:: SubBoard -> Winner
subBoardWinner Complete a = a
subBoardWinner Incomplete spots = 
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

checkVertical :: Location -> [Location] -> Bool
checkVertical a xs = (a `elem` xs) && ((a+3) `elem` xs) && $ (a+6) `elem` xs 
checkHorizontal :: Location -> [Location] -> Bool
checkHorizontal a xs = (a `elem` xs) && ((a+1) `elem` xs) && $ (a+2) `elem xs
checkDiagonalLeft :: [Location] -> Bool
checkDiagonalLeft xs = (0 `elem` xs) && (4 `elem` xs) && (8 `elem` xs)
checkDiagonalRight :: [Location] -> Bool
checkDiagonalRight xs = (2 `elem` xs) && (4 `elem` xs) && (6 `elem` xs)

checkWinner [Location] -> Bool
checkWinner xs = let b1 = checkVertical 0 xs   || checkVertical 1 xs   || checkVertical 2 xs
                     b2 = checkHorizontal 0 xs || checkHorizontal 3 xs || checkHorizontal 6 xs
                     b3 = checkDiagonalLeft xs || checkDiagonalRight xs
                     in b1 || b2 || b3


boardToSubBoard :: Board -> [Spot]
boardToSubBoard [] = []
boardToSubBoard (Complete Player a:xs) =(Full a):(aux xs)
boardToSubBoard (x:xs) = Emp:xs

-- obviously this is just some of my code, you guys feel free to choose what you want from our doc
-- i just put mine in as a placeholder