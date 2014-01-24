module Main (main) where

import System.Environment(getArgs)

import Hangman

main = do
    args <- getArgs
    let filePath = head args
        n = read $ head $ tail args
    wordList <- readWordFile filePath
    displayList $ removeWordsOfWrongLength n wordList
