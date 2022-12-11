import System.IO

computeSingleScore :: (String, String) -> Int
computeSingleScore (x,y)
  | x == "A" && y == "X" = 1 + 3
  | x == "A" && y == "Y" = 2 + 6
  | x == "A" && y == "Z" = 3 + 0
  | x == "B" && y == "X" = 1 + 0
  | x == "B" && y == "Y" = 2 + 3
  | x == "B" && y == "Z" = 3 + 6
  | x == "C" && y == "X" = 1 + 6
  | x == "C" && y == "Y" = 2 + 0
  | x == "C" && y == "Z" = 3 + 3
  | otherwise = 0

parseChoices :: String -> (String, String)
parseChoices str = helper (words str)
  where
    helper [x,y] = (x,y)

main = do
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/02/input.txt" ReadMode
  contents <- hGetContents handle

  -- solution here
  let list = (lines contents)
  let selections = map parseChoices list

  let scores = map computeSingleScore selections
  let total = foldr (+) 0 scores
  print total

  hClose handle
