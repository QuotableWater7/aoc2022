import System.IO
import qualified Data.List as L

computeIndex :: String -> Int -> Int
computeIndex [] _ = 0
computeIndex str num_uniq = computeIndexHelper 0 str num_uniq
  where
    computeIndexHelper n [] _ = n
    computeIndexHelper n list@(x:xs) num_uniq
      | length (L.nub(first_n)) == num_uniq = n + num_uniq
      | otherwise = computeIndexHelper (n+1) xs num_uniq
      where first_n = take num_uniq list

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/06/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let answerPart1 = computeIndex contents 4
  print answerPart1

  -- part 2
  let answerPart2 = computeIndex contents 14
  print answerPart2

  -- clean up
  hClose handle