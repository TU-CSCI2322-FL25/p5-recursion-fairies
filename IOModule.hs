module IOModule where
import DataTypes
import GameCode
import GameText
import GameSolver
import System.IO
-- other io
putBestMove :: GameState -> IO ()
putBestMove state = putStrLn $ printGame stateAfter
    where stateAfter = makeMove state $ bestMove state

writeGameState :: FilePath -> GameState -> IO ()
writeGameState path state = writeFile path $ gameStateOut state

loadGameState :: FilePath -> IO GameState
loadGameState = readGameState

main :: IO ()
main = do
    putStrLn "What is filepath of gameState?"
    path <- getLine
    state <- loadGameState path
    putBestMove state