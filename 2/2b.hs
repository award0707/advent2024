module Advent2b where

import Data.List

load :: FilePath -> IO [[Int]]
load f = readFile f >>= \x -> return $ (map . map) read $ map words (lines x)

check :: (Int -> Int -> Bool) -> [Int] -> Bool
check f (a:b:rest) = if not (f a b) then False else check f (b:rest)
check f _ = True

check2 :: (Int -> Int -> Bool) -> [Int] -> Bool
check2 f (a:b:c:rest) 
  | not (f a b) = check f (a:c:rest) || check f (b:c:rest)
  | not (f b c) = check f (a:c:rest) || check f (a:b:rest)
  | otherwise = check2 f (b:c:rest)
check2 f (a:b:[]) = f a b

countSafe :: [Bool] -> [Bool] -> Int
countSafe xs ys = length $ filter (==True) $ zipWith (||) xs ys

up x y = y - x < 4 && y - x > 0
dn x y = up y x

part1 :: [[Int]] -> Int
part1 xss = countSafe (map (check up) xss) (map (check dn) xss)

part2 :: [[Int]] -> Int
part2 xss = countSafe (map (check2 up) xss) (map (check2 dn) xss)

main :: FilePath -> IO ()
main fn = do
   i <- load fn
   print . part1 $ i
   print . part2 $ i

