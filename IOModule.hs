module IOModule where
import DataTypes
import GameCode
import GameText
import System.IO
-- other io
putBestMove :: GameState -> IO ()
putBestMove state = putStrLn $ printGame stateAfter
    where stateAfter = makeMove state $ bestMove state

writeGameState :: FilePath -> GameState -> IO ()
writeGameState path state = writeFile path $ gameStateOut state