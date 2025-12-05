module Main where
import DataTypes
import GameCode
import GameText
import GameSolver
import System.IO
import System.Console.GetOpt

data Flag = Help | Winner | Depth String | Move Move | Verbose-- | Interactive

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner) "Find the best move for the current player."
          , Option ['d'] ["depth"] (ReqArg Depth "<#>") "Find the best move for the current player with a specified cutoff depth."
          , Option ['m'] ["move"] (ReqArg (Move . read) "<move>") "Make the specified move and print the resulting board."
          , Option ['v'] ["verbose"] (NoArg Verbose) "An option for -move; print the resulting board alongside a rating of the move made."
          -- , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game and play against the computer."
          ]
          
-- other io
putBestMove :: GameState -> IO ()
putBestMove state = putStrLn $ showMove $ bestMove state
    

putState :: GameState -> IO ()
putState state = putStrLn $ showGame state

writeGameState :: FilePath -> GameState -> IO ()
writeGameState path state = writeFile path $ gameStateOut state

loadGameState :: FilePath -> IO GameState
loadGameState = readGameState

-- defaultDepth :: Int
-- defaultDepth = 5
-- main :: IO ()
-- main = do
--   args <- getArgs
--   if null args
--     then putStrLn "Usage: ./game <filename>"
--     else do
--       let filename = head args
--       gameState <- readGameState filename
--       putStrLn $ showMove $ bestMove gameState 
--       -- putStrLn $ showMove $ (whatever we name story 18) gameState defaultDepth

main :: IO ()
main = do
    putStrLn "What is filepath of gameState?"
    path <- getLine
    state <- loadGameState path
    putState state
    putBestMove state
    let newState = makeMove state (bestMove state)
    putState newState