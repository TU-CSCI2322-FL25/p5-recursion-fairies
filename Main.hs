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
putState state = putStrLn $ printGame state

writeGameState :: FilePath -> GameState -> IO ()
writeGameState path state = writeFile path $ gameStateOut state

loadGameState :: FilePath -> IO GameState
loadGameState = readGameState

main :: IO ()
main = do
    putStrLn "What is filepath of gameState?"
    path <- getLine
    state <- loadGameState path
    putState state
    putBestMove state
    let newState = makeMove state (bestMove state)
    putState newState
