module Game where
import Data.List

main :: IO () --basic io file we can use to make game print out after moves etc
main =
    do contents <- readFile "fortunes.txt"
        let fortunes = lines contents
        putStrLn "What is your name?"
        name <- getLine
        putStrLn (getFortune name fortunes)

getFortune :: String -> [String] -> String
getFortunes name fortunes = fortunes !! $ length name