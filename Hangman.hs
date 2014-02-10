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
                then acc
                else acc + 1

removeWordsOfWrongLength :: Int -> [String] -> [String]
removeWordsOfWrongLength n = filter (\string -> length string == n)

matchesPattern :: String -> Char -> [Int] -> Bool
matchesPattern str letter pattern = (elemIndices letter str) == pattern

reduceByPattern :: Char -> [Int] -> [String] -> [String]
reduceByPattern letter pattern = filter (\string -> matchesPattern string letter pattern)

removeWordsWithoutLetter :: Char -> [String] -> [String]
removeWordsWithoutLetter letter = filter (\string -> not . null $ elemIndices letter string)

removeWordsWithLetter :: Char -> [String] -> [String]
removeWordsWithLetter letter = filter (\string -> null $ elemIndices letter string)

-- "america", a   -> [0, 6]
-- "anaconda" , a -> [0, 2, 7]
parsePattern :: String -> Char -> [Int]
parsePattern str letter =
    let (_, ns) = foldl' f (0, []) str
    in reverse ns
        where
            f (n, acc) c = (n + 1, if c == letter then (n:acc) else acc)

patternByLetter :: [String] -> Char -> [[Int]]
patternByLetter strs letter = map f strs
    where
        f str = parsePattern str letter

replaceAt :: String -> Char -> Int -> String
replaceAt source char nth = 
    let (pre, (_:suf)) = splitAt nth source
    in pre ++ (char:suf)

replaceAtByPattern :: String -> Char -> [Int] -> String
replaceAtByPattern source char nths = 
    foldl' (\acc nth -> replaceAt acc char nth) source nths

safeHead :: [[a]] -> [a]
safeHead [] = []
safeHead xs = head xs

mostFreqPatternByLetter :: [String] -> Char -> ([Int], Int)
mostFreqPatternByLetter wordList letter = 
    let wordListWithLetter = removeWordsWithoutLetter letter wordList
        patternList = patternByLetter wordListWithLetter letter {-[ [0] , [0,3], [1,4],...-}
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

    in if (countWordWithoutLetter wordList' letter) > patternCount
       then (removeWordsWithLetter letter wordList', target)
       else funcB wordList' target letter maxPattern
    where
     funcB :: [String] -> String -> Char -> [Int] -> ([String], String)
     funcB wordList' target letter maxPattern =
        let revealedWord = replaceAtByPattern target letter maxPattern
            wordList'' = reduceByPattern letter maxPattern wordList'

        in  (wordList'', revealedWord)
