module Main (main) where

import System.Environment(getArgs)
import System.Exit
import Hangman


main = do
    args <- getArgs
    wordList <- readFile $ head args
    let targetWord = "*******"
    putStrLn targetWord
    mainLoop targetWord $ lines wordList
        where
            mainLoop:: String -> [String] -> IO ()
            mainLoop targetWord wordList = do
                (letter:_) <- getLine
                let (newWordList, revealedWord) = cheatHangMan wordList targetWord letter
                putStrLn revealedWord
                if all (/= '*') revealedWord
                then putStrLn "success"
                else putStrLn "failure" >> mainLoop targetWord newWordList

            cheatHangMan :: [String] -> String -> Char -> ([String], String)
            cheatHangMan _ _ letter = (["ki"], "le*ter")


    -- wordList <- mapM f (lines contents)
    -- mapM_ (putStrLn.snd) wordList
    --     where
    --         f :: String -> IO ([String], String)
    --         f (letter:_) = return (["ki"], "@@@@@")

                
--
--

-- TEST
-- main = do
--     args <- getArgs
--     let (filePath:((letter:_):_)) = args
--     wordList <- readWordFile filePath
--     let p = mostFreqPatternByLetter  wordList  letter
--     putStrLn $ show p

-- TEST
-- main = do
--     args <- getArgs
--     let (filePath:((letter:_):_)) = args
--     wordList <- readWordFile filePath
--     putStrLn $ show $ patternByLetter wordList letter
                                 --
-- TEST
-- main = do
--     args <- getArgs
--     let (filePath:((letter:_):_)) = args
--     wordList <- readWordFile filePath
--     let (p, _) = mostFreqPatternByLetter  wordList  letter
--     mapM_ putStrLn $ map show p
--
