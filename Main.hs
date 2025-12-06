module Main where
import DataTypes
import GameCode
import GameText
import GameSolver
import System.Console.GetOpt
import System.Environment (getArgs)

data Flag = Help | Winner | Depth String | Move Move | Verbose deriving (Eq)-- | Interactive

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner) "Find the best move for the current player."
          , Option ['d'] ["depth"] (ReqArg Depth "<#>") "Find the best move for the current player with a specified cutoff depth."
          , Option ['m'] ["move"] (ReqArg (Move . read) "<move>") "Make the specified move and print the resulting board."
          , Option ['v'] ["verbose"] (NoArg Verbose) "An option for -move; print the resulting board alongside a rating of the move made."
          -- , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game and play against the computer."
          ]
          


--defaultDepth :: Int
--defaultDepth = 5

main :: IO ()
main = do
  args <- getArgs
  let (flags, files, _) = getOpt Permute options args
  
  if Help `elem` flags then
    putStrLn $ usageInfo "Usage: ./main [OPTIONS] <filename>\n" options
  else if null files then
    putStrLn "Usage: ./main <filename>"
  else do
    gameState <- readGameState (head files)
    let verbose = Verbose `elem` flags
        moves = [m | Move m <- flags]
        depths = [d | Depth d <- flags]
    
    if not (null moves) then
      let newState = makeMove gameState (head moves)
      in if verbose then putStrLn $ showGame newState 
                    else putStrLn $ gameStateOut newState
    
    else if Winner `elem` flags then
      if verbose then do
        putStrLn $ "Best move: " ++ showMove (bestMove gameState)
        putStrLn $ "Expected outcome: " ++ prettyWinner (whoWillWin gameState)
      else putStrLn $ showMove (bestMove gameState)
    
    else if not (null depths) then
      --Story 23 - Depth flag (using bestMove for now so we can still compile)
      let _ = read (head depths) :: Int  --We can read the depth, but we are not using it till story 17 is completed
      in putStrLn $ showMove (bestMove gameState)
    
    else
      --Default (using bestMove for now so we can still compile)
      putStrLn $ showMove (bestMove gameState)