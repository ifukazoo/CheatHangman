module Main (main) where

import System.Environment(getArgs)
import Hangman

main = do
    args <- getArgs
    wordList <- readWordFile $ head args
    let targetWord = "********"
    putStrLn targetWord
    mainLoop targetWord wordList
        where
            mainLoop:: String -> [String] -> IO ()
            mainLoop targetWord wordList = do
                (letter:_) <- getLine
                let (newWordList, revealedWord) = cheatHangMan wordList targetWord letter
                putStrLn $ show newWordList
                putStrLn revealedWord
                if all (/= '*') revealedWord
                then putStrLn "success"
                else putStrLn "failure" >> mainLoop revealedWord newWordList
