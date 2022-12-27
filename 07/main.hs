import System.IO
import Data.List.Split

data File = File Int String | Folder String [File] deriving(Show)

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/07/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let lines = splitOn "\n" contents
  print lines

  -- tidy up
  hClose handle