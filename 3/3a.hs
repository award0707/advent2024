module Advent3a where

import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Replace.Megaparsec
import Data.Either
import Data.Void

type Parser = Parsec Void String

load :: FilePath -> IO String 
load = readFile

parseXY :: Parser Int
parseXY = do
   string "mul("
   x <- decimal
   char ','
   y <- decimal
   char ')'
   return (x * y)

findDo :: String -> String
findDo ('d':'o':'n':'\'':'t':'(':')':ss) = findDont ss
findDo (s:ss) = s : findDo ss
findDo [] = []

findDont :: String -> String
findDont ('d':'o':'(':')':ss) = findDo ss
findDont (_:ss) = findDont ss
findDont [] = []

part1 :: String -> Int
part1 = sum . rights . splitCap parseXY

part2 :: String -> Int
part2 = part1 . findDo

main :: FilePath -> IO ()
main fn = do
   i <- load fn
   print . part1 $ i
   print . part2 $ i

