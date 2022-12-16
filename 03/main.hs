import System.IO
import Data.Char
import Control.Monad

getPriority :: Maybe Char -> Maybe Int 
getPriority Nothing = Nothing
getPriority (Just ltr)
  | ord ltr >= ord 'a' = Just(ord ltr - ord 'a' + 1)
  | otherwise = Just(ord ltr - ord 'A' + 27)

firstMutual :: Eq a => [[a]] -> Maybe a
firstMutual [] = Nothing
firstMutual ((x:xs):ys)
  | all (\list -> x `elem` list) ys = Just x
  | otherwise = firstMutual (xs:ys)
firstMutual _ = Nothing

splitRucksack :: String -> [String]
splitRucksack list = [take sacklength list, drop sacklength list]
  where
    sacklength = (length list) `div` 2

splitRucksackPart2 :: [String] -> [[String]]
splitRucksackPart2 [] = []
splitRucksackPart2 (x:y:z:xs) = [[x, y, z]] ++ (splitRucksackPart2 xs)
splitRucksackPart2 _ = []

main = do  
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/03/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rucksacks = lines contents
  let splitRucksacks = map splitRucksack rucksacks
  let firstMutuals = map firstMutual splitRucksacks
  let priorities = fmap getPriority firstMutuals
  let answer = fmap sum $ sequence priorities
  print answer

  -- part 2
  let rucksacksInThirds = splitRucksackPart2 rucksacks
  let firstMutualsPart2 = map firstMutual rucksacksInThirds
  let prioritiesPart2 = fmap getPriority firstMutualsPart2
  let answerPart2 = fmap sum $ sequence prioritiesPart2
  print answerPart2

  -- tie up loose ends
  hClose handle