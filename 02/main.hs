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

getScoreForAction :: TurnAction -> Int
getScoreForAction Rock = 1
getScoreForAction Paper = 2
getScoreForAction Scissors = 3

parseTurn :: String -> Either String (TurnAction, TurnAction)
parseTurn [first_move_str, ' ', second_move_str] = do
  first_turn_action <- parseTurnAction first_move_str
  second_turn_action <- parseTurnAction second_move_str
  Right (first_turn_action, second_turn_action)
parseTurn t = Left $ "Error parsing turn: " ++ t

-- Compare actions and return a score
getScoreFor2ndPlayer :: (TurnAction, TurnAction) -> Int
getScoreFor2ndPlayer (Rock, Rock) = getScoreForTurnResult Tie
getScoreFor2ndPlayer (Rock, Paper) = getScoreForTurnResult Won
getScoreFor2ndPlayer (Rock, Scissors) = getScoreForTurnResult Lost
getScoreFor2ndPlayer (Paper, Rock) = getScoreForTurnResult Lost
getScoreFor2ndPlayer (Paper, Paper) = getScoreForTurnResult Tie
getScoreFor2ndPlayer (Paper, Scissors) = getScoreForTurnResult Won
getScoreFor2ndPlayer (Scissors, Rock) = getScoreForTurnResult Won
getScoreFor2ndPlayer (Scissors, Paper) = getScoreForTurnResult Lost
getScoreFor2ndPlayer (Scissors, Scissors) = getScoreForTurnResult Tie

-- Receive points for the selection and who won the round
computeSingleScorePart1 :: (TurnAction, TurnAction) -> Int
computeSingleScorePart1 tuple = getScoreForAction (snd tuple) + getScoreFor2ndPlayer tuple

-- part 2 helpers

data TurnResult = Won | Lost | Tie deriving(Show)

parseTurnResult :: Char -> Either String TurnResult
parseTurnResult 'X' = Right Lost
parseTurnResult 'Y' = Right Tie
parseTurnResult 'Z' = Right Won
parseTurnResult c = Left $ "Invalid turn result: " ++ [c]

getScoreForTurnResult :: TurnResult -> Int
getScoreForTurnResult Won = 6
getScoreForTurnResult Tie = 3
getScoreForTurnResult Lost = 0

parseTurnAndResult :: String -> Either String (TurnAction, TurnResult)
parseTurnAndResult [move_string, ' ', turn_result_string] = do
  first_move <- parseTurnAction move_string
  turn_result <- parseTurnResult turn_result_string
  Right (first_move, turn_result)
parseTurnAndResult t = Left $ "Error parsing turn: " ++ t

-- When we know the round result but not the second player's choice
get2ndPlayerChoice :: (TurnAction, TurnResult) -> TurnAction
get2ndPlayerChoice (Rock, Won) = Paper
get2ndPlayerChoice (Rock, Tie) = Rock
get2ndPlayerChoice (Rock, Lost) = Scissors
get2ndPlayerChoice (Paper, Won) = Scissors
get2ndPlayerChoice (Paper, Tie) = Paper
get2ndPlayerChoice (Paper, Lost) = Rock
get2ndPlayerChoice (Scissors, Won) = Rock
get2ndPlayerChoice (Scissors, Tie) = Scissors
get2ndPlayerChoice (Scissors, Lost) = Paper

-- Receive points for the selection and who won the round
computeSingleScorePart2 :: (TurnAction, TurnResult) -> Int
computeSingleScorePart2 (first_player_choice, turn_result) = turn_result_score + action_score
  where
    second_player_choice = get2ndPlayerChoice (first_player_choice, turn_result)
    -- computations for result
    turn_result_score = getScoreForTurnResult turn_result
    action_score = getScoreForAction second_player_choice

main = do
  -- read file contents
  handle <- openFile "02/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let list = lines contents
  let turns_part_1 = mapM parseTurn list

  case turns_part_1 of
    Right turns -> do 
      let scoresPart1 = map computeSingleScorePart1 turns
      print $ "Scores part 1: " ++ show (sum scoresPart1)
    Left error -> print "Couldn't parse turns for part 1"

  -- part 2
  let turns_part_2 = mapM parseTurnAndResult list

  case turns_part_2 of
    Right turns -> do
      let scoresPart2 = map computeSingleScorePart2 turns
      print $ "Scores part 2: " ++ show (sum scoresPart2)
    Left error -> print "Couldn't parse turns for part 2"

  hClose handle
