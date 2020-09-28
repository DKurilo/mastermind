module Main where

import           Lib
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "How many games you want to play? (even number)"
    n <- read <$> getLine
    play n
