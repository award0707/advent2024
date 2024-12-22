module Advent1 where

import Data.List
import qualified Data.Map.Strict as Map

load :: FilePath -> IO [Int]
load f = fmap (map read . words) (readFile f)

split :: [a] -> ([a],[a])
split (o:e:xs) = (o:os, e:es) where (os,es) = split xs
split _ = ([],[])

dists :: (Ord a, Num a) => ([a],[a]) -> [a]
dists (a,b) = zipWith (\x y -> abs (x-y)) (sort a) (sort b)

part1 :: [Int] -> Int
part1 = sum . dists . split

countMap :: (Ord k, Num a) => [k] -> Map.Map k a
countMap xs = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty xs

part2 :: [Int] -> Int
part2 input = 
   let (ls,rs) = split input
       m = countMap rs
       occ n = case Map.lookup n m of
                 Just x  -> x
                 Nothing -> 0
    in foldr (\l acc -> acc + l * occ l) 0 ls

main :: FilePath -> IO ()
main fn = load fn >>= \i -> print (part1 i) >> print (part2 i)

