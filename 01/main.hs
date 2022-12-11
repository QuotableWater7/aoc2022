import System.IO  
import Control.Monad

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
  let answer = maximum(map getSumForGroup groups)

  -- show output
  print answer

  -- tie up loose ends
  hClose handle