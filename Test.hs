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
 , test_local 
 , test_local2
 ) where

import Test.HUnit
import Hangman
import Data.List(foldl', elemIndices, group, sort, sortBy, maximumBy)

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
    ]

