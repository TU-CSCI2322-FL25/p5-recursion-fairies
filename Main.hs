module Main where
import DataTypes
import GameCode
import GameText
import GameSolver
import System.IO
-- other io
putBestMove :: GameState -> IO ()
putBestMove state = putStrLn $ showMove $ bestMove state
    

putState :: GameState -> IO ()
putState state = putStrLn $ showGame state

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
