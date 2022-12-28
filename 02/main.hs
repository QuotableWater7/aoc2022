import System.IO

computeSingleScore :: (String, String) -> Either String Int
computeSingleScore (x,y)
  | x == "A" && y == "X" = Right $ 1 + 3
  | x == "A" && y == "Y" = Right $ 2 + 6
  | x == "A" && y == "Z" = Right $ 3 + 0
  | x == "B" && y == "X" = Right $ 1 + 0
  | x == "B" && y == "Y" = Right $ 2 + 3
  | x == "B" && y == "Z" = Right $ 3 + 6
  | x == "C" && y == "X" = Right $ 1 + 6
  | x == "C" && y == "Y" = Right $ 2 + 0
  | x == "C" && y == "Z" = Right $ 3 + 3
  | otherwise = Left $ "Invalid combo: (" ++ x ++ ", " ++ y ++ ")"

computeSingleScorePart2 :: (String, String) -> Either String Int
computeSingleScorePart2 (x,y)
  | x == "A" && y == "X" = Right $ 3 + 0
  | x == "A" && y == "Y" = Right $ 1 + 3
  | x == "A" && y == "Z" = Right $ 2 + 6
  | x == "B" && y == "X" = Right $ 1 + 0
  | x == "B" && y == "Y" = Right $ 2 + 3
  | x == "B" && y == "Z" = Right $ 3 + 6
  | x == "C" && y == "X" = Right $ 2 + 0
  | x == "C" && y == "Y" = Right $ 3 + 3
  | x == "C" && y == "Z" = Right $ 1 + 6
  | otherwise = Left $ "Invalid combo: (" ++ x ++ ", " ++ y ++ ")"

parseChoices :: String -> (String, String)
parseChoices str = helper (words str)
  where
    helper [x,y] = (x,y)

main = do
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/02/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let list = lines contents
  let selections = map parseChoices list

  let answerPart1 = sequence . (map computeSingleScore) $ selections
  case answerPart1 of
    Left error -> print $ "Error: " ++ error
    Right answerPart1 -> print $ show (sum answerPart1)


  -- part 2
  let answerPart2 = sequence . (map computeSingleScorePart2) $ selections
  case answerPart2 of
    Left error -> print $ "Error: " ++ error
    Right answerPart2 -> print $ show (sum answerPart2)

  hClose handle
