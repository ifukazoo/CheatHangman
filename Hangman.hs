module Hangman
(
     readWordFile
   , displayList
   , countWordWithoutLetter
   , removeWordsOfWrongLength
 ) where

import System.IO
import Data.List(foldl')
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
