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

splitRucksack :: String -> Either String [String]
splitRucksack list = if remainder == 0
  then Right [take sacklength list, drop sacklength list]
  else Left "Rucksack length should be divisible by 2"
  where
    sacklength = length list `div` 2
    remainder = length list `mod` 2

splitRucksackPart2 :: [String] -> Either String [[String]]
splitRucksackPart2 [] = Right []
splitRucksackPart2 (x:y:z:xs) = do
  rest_of_solution <- splitRucksackPart2 xs
  Right $ [x, y, z] : rest_of_solution
splitRucksackPart2 _ = Left "Number of lines should be divisible by 3"

main = do
  -- read file contents
  handle <- openFile "03/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rucksacks = lines contents
  let splitRucksacks = mapM splitRucksack rucksacks
  case splitRucksacks of
    Left error -> print $ "Error: " ++ error
    Right splitRucksacks -> do
      let firstMutuals = map firstMutual splitRucksacks
      let priorities = map getPriority firstMutuals
      let answer = sum <$> sequence priorities
      print answer

  -- part 2
  let rucksacksInThirds = splitRucksackPart2 rucksacks
  case rucksacksInThirds of
    Left error -> print $ "Error: " ++ error
    Right rucksacksInThirds -> do
      let firstMutualsPart2 = map firstMutual rucksacksInThirds
      let prioritiesPart2 = fmap getPriority firstMutualsPart2
      let answerPart2 = sum <$> sequence prioritiesPart2
      print answerPart2

  -- tie up loose ends
  hClose handle