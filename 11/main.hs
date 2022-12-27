import System.IO
import Text.Regex.TDFA
import Data.List
import Data.List.Split
import Data.Function

-- Variable
data Variable = DynamicValue | HardcodedValue Int deriving(Show)

parseVariable :: String -> Variable
parseVariable var = if var == "old" then DynamicValue else HardcodedValue (read var)

hydrateVariable :: Variable -> Int -> Int
hydrateVariable (HardcodedValue hv) _ = hv
hydrateVariable (DynamicValue) value = value

-- OpType
data OpType = Add | Multiply deriving(Show)

parseOpType :: String -> OpType
parseOpType op = if op == "+" then Add else if op == "*" then Multiply else error "Invalid opType"

getOperator :: OpType -> (Int -> Int -> Int)
getOperator Add = (+)
getOperator Multiply = (*)

-- Operation
data Operation = Operation OpType Variable Variable

instance Show Operation where
  show (Operation opType var1 var2) = "Op: " ++ (show opType) ++ " (" ++ (show var1) ++ ", " ++ (show var2) ++ ")"

makeOperation :: String -> String -> String -> Operation
makeOperation op var1 var2 = Operation (parseOpType op) (parseVariable var1) (parseVariable var2)

solveOperation :: Operation -> Int -> Int
solveOperation (Operation op var1 var2) value = opFunc hydratedVar1 hydratedVar2
  where
    opFunc = getOperator op
    hydratedVar1 = hydrateVariable var1 value
    hydratedVar2 = hydrateVariable var2 value

-- Monkey
data Monkey = Monkey {
  index :: Int,
  items :: [Int],
  numberOfUpdates :: Int,
  operation :: Operation,
  test :: Int -> Bool,
  testTrue :: Int,
  testFalse :: Int
}

instance Show Monkey where
  show m = "Monkey: \n" ++ "\tIndex:\t" ++ show (index m) ++ "\n\tNum Updates:\t" ++ (show $ numberOfUpdates m) ++ "\n\tItems:\t" ++ show (items m) ++ "\n\t" ++ (show . operation $ m) ++ "\n\tTest True:\t" ++ show (testTrue m) ++ "\n\tTest False:\t" ++ show (testFalse m) ++ "\n"

parseMonkey :: String -> Monkey
parseMonkey str = Monkey { 
  index = (read . last . head) (str =~ "Monkey ([0-9]+):"::[[String]]), 
  items = ((map read) . (splitOn ", ") . last . head) (str =~ "Starting items: (.*)"::[[String]]), 
  numberOfUpdates = 0, 
  operation = makeOperation opType var1 var2, 
  test = (== 0) . (`mod` divisible_by), 
  testTrue = (read . last . head) (str =~ "If true: throw to monkey ([0-9]+)"::[[String]]), 
  testFalse = (read . last . head) (str =~ "If false: throw to monkey ([0-9]+)"::[[String]]) 
}
  where
    opType = (last . head) (str =~ "Operation:.*([+*]).*"::[[String]])
    var1 = (last . head) (str =~ "Operation:.* = ([a-z0-9]+) [+*].*"::[[String]])
    var2 = (last . head) (str =~ "Operation:.*[+*] ([a-z0-9]+).*"::[[String]])
    divisible_by = (read . last . head) (str =~ "Test: divisible by ([0-9]+)"::[[String]])

-- Inspect the first item, which in turn increments the number of updates to the monkey
inspectFirstItem :: Monkey -> (Int, Monkey)
inspectFirstItem monkey = (worry_level, updatedMonkey)
  where
    item = (head . items) monkey
    worry_level = computeWorryLevel monkey item
    
    updatedMonkey = monkey { 
      items = (tail . items) monkey,
      numberOfUpdates = (numberOfUpdates monkey) + 1
    }

-- Adds an item to a monkey's item list
pushItem :: Int -> Monkey -> Monkey
pushItem item monkey = monkey { 
  items = (items monkey) ++ [item] 
}

computeWorryLevel :: Monkey -> Int -> Int
computeWorryLevel monkey item = result
  where
    worryLevelAfterInspection = solveOperation (operation monkey) item
    result = floor ((fromIntegral worryLevelAfterInspection) / 3)

-- Determine index of monkey to throw to next
getMonkeyIndexToThrowTo :: Monkey -> Int -> Int
getMonkeyIndexToThrowTo monkey worry_level = do
  case test monkey $ worry_level of
    True -> testTrue monkey
    False -> testFalse monkey

-- Given an index and a list of monkeys, return tuple with the found monkey and the remaining monkeys
removeMonkeyWithIndex :: Int -> [Monkey] -> Maybe (Monkey, [Monkey])
removeMonkeyWithIndex i monkeys = do
  monkey_with_index_i <- getMonkeyAtIndex i monkeys
  let rest_of_monkeys = filter (not . (== i) . index) monkeys

  return (monkey_with_index_i, rest_of_monkeys)

updateMonkeyAtIndex :: Int -> (Monkey -> Monkey) -> [Monkey] -> Maybe [Monkey]
updateMonkeyAtIndex i updateMonkeyFn monkeys = do
  (monkey_to_update, other_monkeys) <- removeMonkeyWithIndex i monkeys
  let updated_monkey = updateMonkeyFn monkey_to_update
  return $ [updated_monkey] ++ other_monkeys

-- find monkey with the given index
getMonkeyAtIndex :: Int -> [Monkey] -> Maybe Monkey
getMonkeyAtIndex i monkeys = do
  monkey <- find (\m -> index m == i) monkeys
  return monkey

-- MAIN HELPERS

-- Run a single round across all monkeys
runRound :: [Monkey] -> Maybe [Monkey]
runRound monkeys = runRoundHelper 0 monkeys
  where
    runRoundHelper monkey_index monkeys
      -- Base case: the round is over when the monkey index matches the number of monkeys
      | monkey_index == (length monkeys)                    = Just monkeys                  
      | otherwise                                           = do
        (source_monkey, other_monkeys) <- removeMonkeyWithIndex monkey_index monkeys

        if (null (items source_monkey))
          then runRoundHelper (monkey_index + 1) monkeys
          else do
            let (worry_level, updated_monkey) = inspectFirstItem source_monkey

            -- Happy path: pull an item off the current monkey, process it, and add to another monkey
            let source_monkey_index = getMonkeyIndexToThrowTo source_monkey worry_level
            updated_other_monkeys <- updateMonkeyAtIndex source_monkey_index (pushItem worry_level) other_monkeys

            let new_monkey_list = updated_monkey:updated_other_monkeys
            runRoundHelper monkey_index new_monkey_list

-- Run arbitrary number of rounds of monkey turns
runRounds :: Int -> [Monkey] -> Maybe [Monkey]
runRounds 0 monkeys = Just monkeys
runRounds round monkeys = do
  roundResult <- runRound monkeys
  runRounds (round - 1) roundResult

-- Compute the final score
computeScoreFromRounds :: [Monkey] -> Int
computeScoreFromRounds monkeys = do
  let ordered_update_amounts = sortBy (flip compare) $ map numberOfUpdates monkeys
  let top_2_results = take 2 ordered_update_amounts
  let score = foldr (*) 1 top_2_results
  score

main = do
  -- read input
  handle <- openFile "/Users/josephbowler/agora/aoc2022/11/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rawMonkeyStrings = splitOn ("\n\n") contents
  let monkeys = map parseMonkey rawMonkeyStrings
  let updated_monkeys = runRounds 10000 monkeys
  
  case updated_monkeys of
    Just updated_monkeys -> putStrLn $ show (computeScoreFromRounds updated_monkeys)
    Nothing -> putStrLn "Something went wrong..."

  -- part 2
  -- remove "divide by 3", instead modulo the worry level based on the LCM of the "divisible by" values

  -- tidy up
  hClose handle