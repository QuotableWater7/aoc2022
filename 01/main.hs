import System.IO
import Data.List (sortBy)
import Data.Ord (comparing)

splitIntoGroups :: [String] -> [[Int]]
splitIntoGroups [] = []
splitIntoGroups list = splitIntoGroupsHelper [] list
  where 
    splitIntoGroupsHelper current [] = [current]
    splitIntoGroupsHelper current (x:xs)
      | null x = [current] ++ (splitIntoGroups xs)
      | otherwise = splitIntoGroupsHelper ((read x):current) xs

main = do  
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/01/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let list = lines contents
  let totalPerElf = map sum . splitIntoGroups $ list
  let answerPart1 = maximum totalPerElf
  print answerPart1

  -- part 2
  let sortedTotals = sortBy (\x y -> compare y x) totalPerElf
  let answerPart2 = sum . take 3 $ sortedTotals
  print answerPart2

  -- tie up loose ends
  hClose handle