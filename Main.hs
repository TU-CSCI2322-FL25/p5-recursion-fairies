module Main where
import DataTypes
import GameCode
import GameText
import GameSolver
import System.Console.GetOpt
import System.Environment (getArgs)

data Flag = Help | Winner | Depth String | MoveStr String | Verbose | Interactive deriving (Eq)-- | Interactive

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner) "Find the best move for the current player."
          , Option ['d'] ["depth"] (ReqArg Depth "<#>") "Find the best move for the current player with a specified cutoff depth."
          , Option ['m'] ["move"] (ReqArg MoveStr "<move>") "Make the specified move and print the resulting board."
          , Option ['v'] ["verbose"] (NoArg Verbose) "An option for -move; print the resulting board alongside a rating of the move made."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game and play against the computer."
          ]
          


defaultDepth :: Int
defaultDepth = 5

main :: IO ()
main = do
  args <- getArgs
  let (flags, files, _) = getOpt Permute options args
  
  if Help `elem` flags then
    putStrLn $ usageInfo "Usage: ./main [OPTIONS] <filename>\n" options
  else if Interactive `elem` flags then do
    let depths = [d | Depth d <- flags]
    newGame <- readGameState "GameStates/gamestart.txt"
    
    putStrLn "New Game"

    putStrLn "What is your first move (leave blank for robot turn first)"
    mvStr <- getLine
    initMove <- initEnterMove mvStr
    if null depths then ongoingGame newGame initMove defaultDepth else ongoingGame newGame initMove $ read $ head depths
  else if null files then
    putStrLn "Usage: ./main <filename>"
  else do
    gameState <- readGameState (head files)
    let verbose = Verbose `elem` flags
        moveStrs = [m | MoveStr m <- flags]
        depths = [d | Depth d <- flags]
    
    if not (null moveStrs) then
      case parseMove (head moveStrs) of 
        Nothing -> putStrLn "Error: Invalid move format. Use (x,y) format." 
        Just move -> 
          let newState = makeMove gameState move
          in if verbose then putGameState newState 
                        else putStrLn $ gameStateOut newState
    
    else if Winner `elem` flags then
      if verbose then do
        putStrLn $ "Best move: " ++ showMove (bestMove gameState)
        putStrLn $ "Expected outcome: " ++ prettyWinner (whoWillWin gameState)
      else putStrLn $ showMove (bestMove gameState)
    
    else if not (null depths) then
      --Story 23 - Depth flag (using bestMove for now so we can still compile)
      let d = read (head depths) :: Int  --We can read the depth, but we are not using it till story 17 is completed
      in putStrLn $ showMove $ snd (whoMightWin gameState d 0)
    
    else
      --Default (using bestMove for now so we can still compile)
      putStrLn $ showMove $ snd (whoMightWin gameState defaultDepth 0)

ongoingGame :: GameState -> Maybe Move -> Int -> IO () --do Nothing for move to let computer go first
ongoingGame state Nothing d = do
  roboTurn state d
ongoingGame state (Just move) d = do
  validMove <- validityMove state move
  let stateAfter = makeMove state validMove
  putGameState stateAfter
  case checkWinner stateAfter of
    Nothing -> roboTurn stateAfter d
    Just w -> putStrLn ("Player " ++ prettyWinner w ++ " wins")

roboTurn :: GameState -> Int -> IO ()
roboTurn state d = do
  putStrLn "Robot deciding best move"
  let roboState = makeMove state $ snd $ whoMightWin state d 0
  putGameState roboState

  case checkWinner roboState of
    Nothing -> do
      putStrLn "Input next move"
      mvStr <- getLine
      nMove <- enterMove mvStr
      ongoingGame roboState (Just nMove) d
    Just w -> putStrLn ("Player " ++ prettyWinner w ++ " wins")

initEnterMove :: String -> IO (Maybe Move)
initEnterMove [] = do
  putStrLn "Computer going first"
  return Nothing
initEnterMove mv =
  case parseMove mv of
    Nothing -> do
      putStrLn "Re-enter move with correct syntax"
      nMv <- getLine
      initEnterMove nMv
    Just nMv -> do
      return $ Just nMv

enterMove :: String -> IO Move
enterMove mv = do
  case parseMove mv of
    Nothing -> do
      putStrLn "Re-enter move with correct syntax"
      nMv <- getLine
      enterMove nMv
    Just nMv -> do
      return nMv

validityMove :: GameState -> Move -> IO Move
validityMove state@(board, player, forced) mv@(a,b) =
  case forced of
    Nothing -> do return mv
    _ ->
      if forced == Just a then do return mv else do
        putStrLn "Move must be within the forced subBoard"
        mv2 <- enterMove ""
        validityMove state mv2