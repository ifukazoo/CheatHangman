module Main (main) where

import System.Environment(getArgs)

import Hangman

main = do
    args <- getArgs
    let (filePath:((letter:_):_)) = args
    wordList <- readWordFile filePath
    displayList $ map f (patternByLetter (removeWordsWithoutLetter letter wordList) letter)
                        where
                             f ints = show ints
