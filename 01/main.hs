import System.IO
import Data.List.Split (splitOn)
import Data.List (sort)
import Data.Ord (comparing)

main = do  
  -- read file contents
  handle <- openFile "01/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let list = lines contents
  let lineGroups = splitOn [""] list
  let numberGroups = map (map read) lineGroups
  let unsortedResults = map sum numberGroups
  let sortedResults = (reverse . sort) unsortedResults
  let max = head sortedResults
  print max
  
  -- part 2
  let best3 = take 3 sortedResults
  print $ sum best3 

  -- tie up loose ends
  hClose handle