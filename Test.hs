--
-- テスト方法
--
-- ghci> :load Test.hs 
-- [2 of 2] Compiling Test             ( Test.hs, interpreted )
-- Ok, modules loaded: Test, Hangman.
-- ghci> mapM_ runTestTT test_mostFreqPatternByLetter 

module Test
(
 test_mostFreqPatternByLetter
 , test_patternByLetter 
 , test_replaceAt 
 , test_replaceAtByPattern 
 , test_reduceByPattern 
 , test_mathchesPattern 
 , test_cheatHangMan 
 , test_local 
 , test_local2
 ) where

import Test.HUnit
import Hangman
import Data.List(  group, sort, maximumBy)

test_cheatHangMan =
    [
      TestCase(assertEqual "cheatHangMan"
              (["jujus", "justs", "jutty", "rusts", "rusty", "rutty", "strut", "sturt", "truss", "trust", "tryst", "tutty", "tutus", "usury", "wurst", "xysts", "yurts"], "*****")
              (cheatHangMan ["jujus", "justs", "jutty", "rusts", "rusty", "rutty", "strut", "sturt", "truss", "trust", "tryst", "tutty", "tutus", "usury", "wurst", "xysts", "yurts"]  "*****" 'q'))
      , TestCase(assertEqual "cheatHangMan"
              True
              (let wordList' = removeWordsOfWrongLength (length "*****") ["jujus", "justs", "jutty", "rusts", "rusty", "rutty", "strut", "sturt", "truss", "trust", "tryst", "tutty", "tutus", "usury", "wurst", "xysts", "yurts"]
                   (maxPattern, patternCount) = mostFreqPatternByLetter wordList' 'q'
              in  (countWordWithoutLetter wordList' 'q') > patternCount))
      , TestCase(assertEqual "cheatHangMan"
              (length  ["jujus", "justs", "jutty", "rusts", "rusty", "rutty", "strut", "sturt", "truss", "trust", "tryst", "tutty", "tutus", "usury", "wurst", "xysts", "yurts"])
              (countWordWithoutLetter  ["jujus", "justs", "jutty", "rusts", "rusty", "rutty", "strut", "sturt", "truss", "trust", "tryst", "tutty", "tutus", "usury", "wurst", "xysts", "yurts"] 'q'))
    ]

test_mathchesPattern =
    [
      TestCase(assertEqual "matchesPattern"
              False
              (matchesPattern "abcaafg" 'a' [0]))
    ]
test_reduceByPattern  =
    [
      TestCase(assertEqual "reduceByPattern"
              ["abcdefg", "abcdefg"]
              (reduceByPattern 'a' [0, 3] [
               "abcaefg"
               , "abcdefg"
               , "abcaefg"
               , "abcdefg"]))
      , TestCase(assertEqual "reduceByPattern"
              ["abcaefg", "abcaafg"]
              (reduceByPattern 'a' [0] [
               "abcaefg"
               , "abcdefg"
               , "abcaafg"
               , "abcdef@"]))
    ]
test_replaceAtByPattern =
    [
      TestCase(assertEqual "replaceAtByPattern"
              "@b@defg"
              (replaceAtByPattern "abcdefg" '@' [0, 2]))
      , TestCase(assertEqual "replaceAtByPattern"
              "@bcdefg"
              (replaceAtByPattern "abcdefg" '@' [0]))
    ]
test_replaceAt =
    [
      TestCase(assertEqual "replaceAt"
              "ab@defg"
              (replaceAt "abcdefg" '@' 2))
      , TestCase(assertEqual "replaceAt"
              "@bcdefg"
              (replaceAt "abcdefg" '@' 0))
    ]

compfunc = (\l r -> compare (length l) (length r))
maxPatterns [] = []
maxPatterns xs = maximumBy compfunc $ (group . sort) xs {-[ [0,3],[0,3],[0,3],[0,3],.. -}

test_local =
    [
      TestCase(assertEqual "local"
              [[0],[0]]
              (maxPatterns $ patternByLetter [ "acceb" , "accdb" , "bccab", "b" ]  'a'))
    ]
test_local2 =
    [
      TestCase(assertEqual "local"
              2
              (length $ maxPatterns $ patternByLetter [ "acceb" , "accdb" , "bccab", "b" ]  'a'))
    ]

test_patternByLetter =
    [
      TestCase(assertEqual "patternByLetter"
              [[0],[0],[3],[]]
              (patternByLetter [ "acceb" , "accdb" , "bccab", "b" ]  'a'))

      , TestCase(assertEqual "patternByLetter"
              [[0, 3],[0, 3],[3],[]]
              (patternByLetter [ "accab" , "accab" , "bccab" , "b"]  'a'))
    ]

test_mostFreqPatternByLetter = 
    [
      TestCase(assertEqual "mostFreqPatternByLetter"
              ([0], 2)
              (mostFreqPatternByLetter [ "acceb" , "accdb" , "bccab", "b" ]  'a'))

    , TestCase(assertEqual "mostFreqPatternByLetter"
            ([0, 3], 2)
            (mostFreqPatternByLetter [ "accab" , "accab" , "bccab" , "b"]  'a')) 
    , TestCase(assertEqual "mostFreqPatternByLetter"
            ([], 0)
            (mostFreqPatternByLetter ["jujus", "justs", "jutty", "rusts", "rusty", "rutty", "strut", "sturt", "truss", "trust", "tryst", "tutty", "tutus", "usury", "wurst", "xysts", "yurts"] 'q'))
    ]

