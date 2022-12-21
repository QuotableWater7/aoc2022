import System.IO
import Text.Regex.TDFA
import Data.List
import Data.List.Split
import Data.Function

-- Variable
data Variable = DynamicValue | HardcodedValue Int deriving(Show)

hydrateVariable :: Variable -> Int -> Int
hydrateVariable (HardcodedValue hv) _ = hv
hydrateVariable (DynamicValue) value = value

data OpType = Multiply | Add deriving(Show)

-- Operation
data Operation = Operation OpType Variable Variable

makeOperation :: String -> String -> String -> Operation
makeOperation opStr str1 str2 = Operation opType var1 var2
  where
    opType = if opStr == "+" then Add else Multiply
    var1 = if str1 == "old" then DynamicValue else HardcodedValue ((read str2)::Int)
    var2 = if str2 == "old" then DynamicValue else HardcodedValue ((read str2)::Int)

convertOperationToExpression :: Operation -> Int -> Expression
convertOperationToExpression (Operation opType var1 var2) value = Expression opType (hydrateVariable var1 value) (hydrateVariable var2 value)

instance Show Operation where
  show (Operation opType var1 var2) = "Op: " ++ (show opType) ++ " (" ++ (show var1) ++ ", " ++ (show var2) ++ ")"
  
-- expression
data Expression = Expression OpType Int Int

instance Show Expression where
  show (Expression opType value1 value2) = "Exp: " ++ (show opType) ++ " (" ++ (show value1) ++ ", " ++ (show value2) ++ ")"

processExpression :: Expression -> Int
processExpression (Expression Add x y) = x + y
processExpression (Expression Multiply x y) = x * y

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
parseMonkey str = Monkey { index=index, items=items, numberOfUpdates=0, operation=operation, test=test, testTrue=testTrue, testFalse=testFalse }
  where
    index = ((read :: String -> Int) . last . head) (str =~ "Monkey ([0-9]+):"::[[String]])
    items = ((map (read :: String->Int)) . (splitOn ", ") . last . head) (str =~ "Starting items: (.*)"::[[String]])
    opType = (last . head) (str =~ "Operation:.*([+*]).*"::[[String]])
    var1 = (last . head) (str =~ "Operation:.* = ([a-z0-9]+) [+*].*"::[[String]])
    var2 = (last . head) (str =~ "Operation:.*[+*] ([a-z0-9]+).*"::[[String]])
    operation = makeOperation opType var1 var2
    divisible_by = ((read :: String->Int) . last . head) (str =~ "Test: divisible by ([0-9]+)"::[[String]])
    test = (== 0) . (`mod` divisible_by)
    testTrue = ((read :: String->Int) . last . head) (str =~ "If true: throw to monkey ([0-9]+)"::[[String]])
    testFalse = ((read :: String->Int) . last . head) (str =~ "If false: throw to monkey ([0-9]+)"::[[String]])

-- Remove the first item, which in turn increments the number of updates to the monkey
removeFirstItem :: Monkey -> (Int, Monkey)
removeFirstItem monkey = (item, updatedMonkey)
  where
    item = (head . items) monkey
    updatedMonkey = Monkey { 
      index = index monkey, 
      items = (tail . items) monkey,
      numberOfUpdates = (numberOfUpdates monkey) + 1,
      operation = operation monkey,
      test = test monkey,
      testTrue = testTrue monkey,
      testFalse = testFalse monkey
    }

-- Adds an item to a monkey's item list
pushItem :: Monkey -> Int -> Monkey
pushItem monkey item = Monkey { 
  index = index monkey, 
  items = (items monkey) ++ [item],
  numberOfUpdates = numberOfUpdates monkey,
  operation = operation monkey,
  test = test monkey,
  testTrue = testTrue monkey,
  testFalse = testFalse monkey
}

computeWorryLevel :: Monkey -> Int -> Int
computeWorryLevel monkey item = result
  where
    worryLevelAfterInspection = processExpression $ convertOperationToExpression (operation monkey) item
    result = floor ((fromIntegral worryLevelAfterInspection) / 3)

-- Determine index of monkey to throw to next
getMonkeyIndexToThrowTo :: Monkey -> Int -> Int
getMonkeyIndexToThrowTo monkey worry_level = do
  case test monkey $ worry_level of
    True -> testTrue monkey
    False -> testFalse monkey

-- Given an index and a list of monkeys, return tuple with the found monkey and the remaining monkeys
removeMonkeyAtIndex :: Int -> [Monkey] -> (Monkey, [Monkey])
removeMonkeyAtIndex i [] = error ("Did not find monkey at index " ++ (show i))
removeMonkeyAtIndex i monkeys = 
  case find ((== i) . index) monkeys of 
    Nothing -> error "no monke"
    Just m -> (m, filter (not . (== i) . index) monkeys)

-- Run a single round across all monkeys
runRound :: Int -> [Monkey] -> [Monkey]
runRound i monkeys
    -- Base case: the round is over when the monkey index matches the number of monkeys
  | i == (length monkeys)           = monkeys                  
    -- Base case: when there are no more items for the current monkey, increase the monkey index
  | items monkey_to_update == []    = runRound (i + 1) monkeys 
    -- Happy path: pull an item off the current monkey, process it, and add to another monkey
  | otherwise                       = do
    let (item, updated_monkey) = removeFirstItem monkey_to_update
    let worry_level = computeWorryLevel monkey_to_update item

    let monkey_to_update_index = getMonkeyIndexToThrowTo monkey_to_update worry_level
    let (other_monkey_to_update, untouched_monkeys) = removeMonkeyAtIndex monkey_to_update_index other_monkeys
    let updated_other_monkey_to_update = pushItem other_monkey_to_update worry_level
    runRound i ([updated_monkey, updated_other_monkey_to_update] ++ untouched_monkeys)
  where
    (monkey_to_update, other_monkeys) = removeMonkeyAtIndex i monkeys

-- Run arbitrary number of rounds of monkey turns
runRounds :: Int -> [Monkey] -> [Monkey]
runRounds 0 monkeys = monkeys
runRounds round monkeys = runRounds (round - 1) (runRound 0 monkeys)

main = do
  -- read input
  handle <- openFile "/Users/josephbowler/agora/aoc2022/11/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rawMonkeyStrings = splitOn ("\n\n") contents
  let monkeys = map parseMonkey rawMonkeyStrings
  let updated_monkeys = runRounds 20 monkeys
  let number_of_updates = sortBy (flip compare) $ map numberOfUpdates updated_monkeys
  let answer_part_1 = (foldr (*) 1) . take 2 $ number_of_updates
  print answer_part_1

  -- tidy up
  hClose handle