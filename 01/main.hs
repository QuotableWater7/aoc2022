import System.IO
import Data.List (sortBy)
import Data.Ord (comparing)

getSumForGroup :: [Int] -> Int
getSumForGroup [x] = x
getSumForGroup (x:xs) = x + getSumForGroup(xs)

splitIntoGroups :: [Int] -> [String] -> [[Int]]
splitIntoGroups current [] = [current]
splitIntoGroups current (x:xs) 
  | null x = current:(splitIntoGroups [] xs)
  | otherwise = splitIntoGroups ((read x):current) xs

main = do  
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/01/input.txt" ReadMode
  contents <- hGetContents handle

  -- process input
  let list = (lines contents)
  let groups = splitIntoGroups [] list
  let totalPerElf = map getSumForGroup groups
  let answer = maximum(totalPerElf)

  -- show output
  print answer

  -- part 2
  let sortedTotals = take 3 (sortBy (\x y -> compare y x) totalPerElf)
  print (foldr (+) 0 sortedTotals)

  -- tie up loose ends
  hClose handle