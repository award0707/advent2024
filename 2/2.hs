module Advent2 where

import Data.List

load :: FilePath -> IO [[Int]]
load f = readFile f >>= \x -> return $ (map . map) read $ map words (lines x)

-- test input
ti :: IO ()
ti = load "example" >>= print

checkDn :: [Int] -> Bool
checkDn (a:b:rest)
  | a - b > 3 = False
  | a - b < 1 = False
  | otherwise = checkDn (b:rest)
checkDn _ = True

checkUp :: [Int] -> Bool
checkUp (a:b:rest)
  | b - a > 3 = False
  | b - a < 1 = False
  | otherwise = checkUp (b:rest)
checkUp _ = True

countSafes :: [Bool] -> Int
countSafes = length . filter (==True)

part1 :: [[Int]] -> Int
part1 xss = countSafes ups + countSafes downs
   where ups = map checkUp xss
         downs = map checkDn xss

main :: FilePath -> IO ()
main fn = load fn >>= print . part1 

