module Hangman
(
     readWordFile
   , displayList
   , countWordWithoutLetter
   , removeWordsOfWrongLength
   , matchesPattern
   , removeWordsWithoutLetter
   , removeWordsWithLetter
   , patternByLetter
   , mostFreqPatternByLetter
   , cheatHangMan
   , replaceAt
   , replaceAtByPattern
   , reduceByPattern
 ) where

import Data.List(foldl', elemIndices, group, sort, maximumBy)

lines' :: String -> [String]
lines' str = map (\s -> if last s  == '\r' then init s  else s) $ lines str

readWordFile :: String -> IO [String]
readWordFile filePath = do
    contents <- readFile filePath
    return $ lines' contents

displayList :: [String] -> IO ()
displayList = mapM_ putStrLn

countWordWithoutLetter :: [String] -> Char -> Int
countWordWithoutLetter xs c  = foldl' f 0 xs
    where
        f acc string = 
            if any (\ch -> ch == c) string
                then acc
                else acc + 1

removeWordsOfWrongLength :: Int -> [String] -> [String]
removeWordsOfWrongLength n = filter (\string -> length string == n)

matchesPattern :: String -> (Char,[Int]) -> Bool
matchesPattern str (letter, pattern) = (elemIndices letter str) == pattern

reduceByPattern :: (Char,[Int]) -> [String] -> [String]
reduceByPattern pair = filter (\str -> matchesPattern str pair)

removeWordsWithoutLetter :: Char -> [String] -> [String]
removeWordsWithoutLetter letter = filter (\string -> not . null $ elemIndices letter string)

removeWordsWithLetter :: Char -> [String] -> [String]
removeWordsWithLetter letter = filter (\string -> null $ elemIndices letter string)

patternByLetter :: Char -> [String] -> [[Int]]
patternByLetter letter = map (elemIndices letter)

replaceAt :: String -> Char -> Int -> String
replaceAt source letter nth = 
    let (pre, (_:suf)) = splitAt nth source
    in pre ++ (letter:suf)

replaceAtByPattern :: String -> (Char,[Int]) -> String
replaceAtByPattern source (letter, pattern) = 
    foldl' (\acc nth -> replaceAt acc letter nth) source pattern

safeHead :: [[a]] -> [a]
safeHead [] = []
safeHead xs = head xs

mostFreqPatternByLetter :: [String] -> Char -> ([Int], Int)
mostFreqPatternByLetter wordList letter = 
    let wordListWithLetter = removeWordsWithoutLetter letter wordList
        patternList = patternByLetter letter wordListWithLetter {-[ [0] , [0,3], [1,4],...-}
        maxPattern = maxNumberElement patternList
    in  (safeHead maxPattern, length maxPattern)
    where
        compfunc = (\l r -> compare (length l) (length r))
        maxNumberElement [] = []
        maxNumberElement xs = maximumBy compfunc $ (group . sort) xs {-[ [0,3],[0,3],[0,3],[0,3],.. -}

cheatHangMan :: [String] -> String -> Char -> ([String], String)
cheatHangMan wordList target letter =
    let wordList' = removeWordsOfWrongLength (length target) wordList
        (maxPattern, patternCount) = mostFreqPatternByLetter wordList' letter
        withoutLetterCount = countWordWithoutLetter wordList' letter 

    in if withoutLetterCount > patternCount
       then (removeWordsWithLetter letter wordList', target)
       else funcElse wordList' target (letter,maxPattern)
    where
     funcElse :: [String] -> String -> (Char,[Int]) -> ([String], String)
     funcElse wordList' target pair =
        let revealedWord = replaceAtByPattern target pair
            wordList'' = reduceByPattern pair wordList'

        in  (wordList'', revealedWord)
