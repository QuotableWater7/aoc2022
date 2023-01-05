import System.IO

data TurnAction = Rock | Paper | Scissors deriving(Show)

parseTurnAction :: Char -> Either String TurnAction
parseTurnAction 'A' = Right Rock
parseTurnAction 'X' = Right Rock
parseTurnAction 'B' = Right Paper
parseTurnAction 'Y' = Right Paper
parseTurnAction 'C' = Right Scissors
parseTurnAction 'Z' = Right Scissors
parseTurnAction str = Left $ "Not a proper turn action: " ++ [str]

parseTurn :: String -> Either String (TurnAction, TurnAction)
parseTurn [first_move_str, ' ', second_move_str] = do
  first_turn_action <- parseTurnAction first_move_str
  second_turn_action <- parseTurnAction second_move_str
  Right (first_turn_action, second_turn_action)
parseTurn t = Left $ "Error parsing turn: " ++ t

computeSingleScorePart1 :: (TurnAction, TurnAction) -> Int
computeSingleScorePart1 (Rock, Rock) = 1 + 3
computeSingleScorePart1 (Rock, Paper) = 2 + 6
computeSingleScorePart1 (Rock, Scissors) = 3 + 0
computeSingleScorePart1 (Paper, Rock) = 1 + 0
computeSingleScorePart1 (Paper, Paper) = 2 + 3
computeSingleScorePart1 (Paper, Scissors) = 3 + 6
computeSingleScorePart1 (Scissors, Rock) = 1 + 6
computeSingleScorePart1 (Scissors, Paper) = 2 + 0
computeSingleScorePart1 (Scissors, Scissors) = 3 + 3

computeSingleScorePart2 :: (TurnAction, TurnAction) -> Int
computeSingleScorePart2 (Rock, Rock) = 3 + 0
computeSingleScorePart2 (Rock, Paper) = 1 + 3
computeSingleScorePart2 (Rock, Scissors) = 2 + 6
computeSingleScorePart2 (Paper, Rock) = 1 + 0
computeSingleScorePart2 (Paper, Paper) = 2 + 3
computeSingleScorePart2 (Paper, Scissors) = 3 + 6
computeSingleScorePart2 (Scissors, Rock) = 2 + 0
computeSingleScorePart2 (Scissors, Paper) = 3 + 3
computeSingleScorePart2 (Scissors, Scissors) = 1 + 6

main = do
  -- read file contents
  handle <- openFile "/Users/josephbowler/agora/aoc2022/02/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let list = lines contents
  let turns = mapM parseTurn list

  case turns of
    Right turns -> do 
      -- part 1
      let scoresPart1 = map computeSingleScorePart1 turns
      print $ "Scores part 1: " ++ show (sum scoresPart1)
      -- part 2
      let scoresPart2 = map computeSingleScorePart2 turns
      print $ "Scores part 2: " ++ show (sum scoresPart2)
    Left error -> print "Couldn't parse turns for part 1"

  hClose handle
