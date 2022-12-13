import System.IO
import qualified Data.List as L

computeIndex :: String -> Int
computeIndex [] = 0
computeIndex str = computeIndexHelper 0 str
  where
    computeIndexHelper n [] = n
    computeIndexHelper n (w:x:y:z:xs)
      | length (L.nub([w, x, y, z])) == 4 = n + 4 -- L.nub removes duplicates
      | otherwise = computeIndexHelper (n + 1) (x:y:z:xs)
    computeIndexHelper n _ = 0

computeIndexPart2 :: String -> Int
computeIndexPart2 [] = 0
computeIndexPart2 str = computeIndexHelperPart2 0 str
  where
    computeIndexHelperPart2 n [] = n
    computeIndexHelperPart2 n list@(x:xs) = case (length_first_fourteen == 14) of 
      True -> case (length (L.nub(first_fourteen)) == 14) of
          True -> n + 14
          False -> computeIndexHelperPart2 (n + 1) xs
      False -> 0
      where first_fourteen = take 14 list
            length_first_fourteen = length first_fourteen

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/06/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let answerPart1 = computeIndex contents
  print answerPart1

  -- part 2
  let answerPart2 = computeIndexPart2 contents
  print answerPart2

  -- clean up
  hClose handle