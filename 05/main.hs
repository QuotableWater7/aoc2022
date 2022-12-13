import System.IO

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/05/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  print contents

  -- clean up
  hClose handle