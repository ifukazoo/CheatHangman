module Hangman
(
     readWordFile
   , displayList
   , countWordWithoutLetter
   , removeWordsOfWrongLength
   , matchesPattern
   , removeWordsWithoutLetter
   , removeWordsWithLetter
 ) where

import System.IO
import Data.List(foldl', elemIndices)
import Control.Monad(mapM_)

myLines :: String -> [String]
myLines str = map (\s -> if last s  == '\r' then init s  else s) $ lines str

readWordFile :: String -> IO [String]
readWordFile filePath = do
    contents <- readFile filePath
    return $ myLines contents

displayList :: [String] -> IO ()
displayList = mapM_ putStrLn

countWordWithoutLetter :: [String] -> Char -> Int
countWordWithoutLetter xs c  = foldl' f 0 xs
    where
        f acc string = 
            if any (\ch -> ch == c) string
                then acc + 1
                else acc

removeWordsOfWrongLength :: Int -> [String] -> [String]
removeWordsOfWrongLength n = filter (\string -> length string == n)

numberInPattern:: [Int] -> Int -> Bool
numberInPattern [] _ = False
numberInPattern (x:xs) n =
    if x == n then True
              else numberInPattern xs n

matchLetter :: String -> Char -> Int -> Bool
matchLetter str letter n
    | n > (length str) -1 = False
    | otherwise = str !! n  == letter

matchesPattern :: String -> Char -> [Int] -> Bool
matchesPattern str letter numbers =
    foldl' function True numbers
        where
            function False _ = False
            function acc n   = matchLetter str letter n

removeWordsWithoutLetter :: Char -> [String] -> [String]
removeWordsWithoutLetter letter = filter (\string -> not . null $ elemIndices letter string)

removeWordsWithLetter :: Char -> [String] -> [String]
removeWordsWithLetter letter = filter (\string -> null $ elemIndices letter string)
