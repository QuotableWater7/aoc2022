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

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/06/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let answerPart1 = computeIndex contents
  print answerPart1

  -- clean up
  hClose handle