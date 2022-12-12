import System.IO
import Data.Char
import Control.Monad

firstMutual :: Eq a => [a] -> [a] -> Maybe a
firstMutual [] _ = Nothing
firstMutual _ [] = Nothing
firstMutual (x:xs) ys
  | x `elem` ys = Just x
  | otherwise = firstMutual xs ys

splitRucksack :: String -> (String, String)
splitRucksack list = ((take sacklength list), (drop sacklength list))
  where
    sacklength = (length list) `div` 2

getPriority :: Maybe Char -> Maybe Int 
getPriority Nothing = Nothing
getPriority (Just ltr)
  | ord ltr >= 97 = Just((ord ltr) - 96)
  | otherwise = Just((ord ltr) - 65 + 27)

main = do  
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/03/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rucksacks = lines contents
  let splitRucksacks = map splitRucksack rucksacks
  let firstMutuals = map (\(a, b) -> (firstMutual a b)) splitRucksacks
  let priorities = fmap getPriority firstMutuals
  let answer = fmap sum $ sequence priorities
  print answer

  -- tie up loose ends
  hClose handle