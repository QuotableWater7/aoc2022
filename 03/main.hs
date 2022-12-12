import System.IO
import Data.Char
import Control.Monad

getPriority :: Maybe Char -> Maybe Int 
getPriority Nothing = Nothing
getPriority (Just ltr)
  | ord ltr >= 97 = Just((ord ltr) - 96)
  | otherwise = Just((ord ltr) - 65 + 27)

-- part 1 helpers

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

-- part 2 helpers

firstMutualPart2 :: Eq a => [a] -> [a] -> [a] -> Maybe a
firstMutualPart2 [] _ _ = Nothing
firstMutualPart2 _ [] _ = Nothing
firstMutualPart2 _ _ [] = Nothing
firstMutualPart2 (x:xs) ys zs
  | x `elem` ys && x `elem` zs = Just x
  | otherwise = firstMutualPart2 xs ys zs

splitRucksackPart2 :: [String] -> [(String, String, String)]
splitRucksackPart2 [] = []
splitRucksackPart2 (x:y:z:xs) = [(x, y, z)] ++ splitRucksackPart2 xs

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

  -- part 2
  let rucksacksInThirds = splitRucksackPart2 rucksacks
  let firstMutualsPart2 = map (\(a, b, c) -> (firstMutualPart2 a b c)) rucksacksInThirds
  let prioritiesPart2 = fmap getPriority firstMutualsPart2
  let answerPart2 = fmap sum $ sequence prioritiesPart2
  print answerPart2

  -- tie up loose ends
  hClose handle