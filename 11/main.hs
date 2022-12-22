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
data OpType = Multiply | Add deriving(Show)

parseOpType :: String -> OpType
parseOpType op = if op == "+" then Add else Multiply

-- Operation
data Operation = Operation OpType Variable Variable

instance Show Operation where
  show (Operation opType var1 var2) = "Op: " ++ (show opType) ++ " (" ++ (show var1) ++ ", " ++ (show var2) ++ ")"

makeOperation :: String -> String -> String -> Operation
makeOperation op var1 var2 = Operation (parseOpType op) (parseVariable var1) (parseVariable var2)

solveOperation :: Operation -> Int -> Int
solveOperation (Operation Add var1 var2) value = (hydrateVariable var1 value) + (hydrateVariable var2 value)
solveOperation (Operation Multiply var1 var2) value = (hydrateVariable var1 value) * (hydrateVariable var2 value)

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
    index = (read . last . head) (str =~ "Monkey ([0-9]+):"::[[String]])
    items = ((map read) . (splitOn ", ") . last . head) (str =~ "Starting items: (.*)"::[[String]])
    opType = (last . head) (str =~ "Operation:.*([+*]).*"::[[String]])
    var1 = (last . head) (str =~ "Operation:.* = ([a-z0-9]+) [+*].*"::[[String]])
    var2 = (last . head) (str =~ "Operation:.*[+*] ([a-z0-9]+).*"::[[String]])
    operation = makeOperation opType var1 var2
    divisible_by = (read . last . head) (str =~ "Test: divisible by ([0-9]+)"::[[String]])
    test = (== 0) . (`mod` divisible_by)
    testTrue = (read . last . head) (str =~ "If true: throw to monkey ([0-9]+)"::[[String]])
    testFalse = (read . last . head) (str =~ "If false: throw to monkey ([0-9]+)"::[[String]])

-- Remove the first item, which in turn increments the number of updates to the monkey
removeFirstItem :: Monkey -> (Int, Monkey)
removeFirstItem monkey = (item, updatedMonkey)
  where
    item = (head . items) monkey
    updatedMonkey = monkey { 
      items = (tail . items) monkey,
      numberOfUpdates = (numberOfUpdates monkey) + 1
    }

-- Adds an item to a monkey's item list
pushItem :: Monkey -> Int -> Monkey
pushItem monkey item = monkey { 
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
removeMonkeyWithIndex :: Int -> [Monkey] -> (Monkey, [Monkey])
removeMonkeyWithIndex i [] = error ("Did not find monkey at index " ++ (show i))
removeMonkeyWithIndex i monkeys = (monkey_with_index_i, rest_of_monkeys)
  where
    monkey_with_index_i = getMonkeyAtIndex i monkeys
    rest_of_monkeys = filter (not . (== i) . index) monkeys

-- find monkey with the given index
getMonkeyAtIndex :: Int -> [Monkey] -> Monkey
getMonkeyAtIndex i monkeys = do
  let monkey = find (\m -> index m == i) monkeys
  case monkey of
    Nothing -> error "invalid index for monkey"
    Just m -> m

-- MAIN HELPERS

-- Run a single round across all monkeys
runRound :: [Monkey] -> [Monkey]
runRound monkeys = runRoundHelper 0 monkeys
  where
    runRoundHelper monkey_index monkeys
      -- Base case: the round is over when the monkey index matches the number of monkeys
      | monkey_index == (length monkeys)                    = monkeys                  
      -- Base case: when there are no more items for the current monkey, increase the monkey index
      | items (getMonkeyAtIndex monkey_index monkeys) == [] = runRoundHelper (monkey_index + 1) monkeys 
      -- Happy path: pull an item off the current monkey, process it, and add to another monkey
      | otherwise                                           = do
        let (monkey_to_update, other_monkeys) = removeMonkeyWithIndex monkey_index monkeys
        let (item, updated_monkey) = removeFirstItem monkey_to_update
        let worry_level = computeWorryLevel monkey_to_update item

        let monkey_to_update_index = getMonkeyIndexToThrowTo monkey_to_update worry_level
        let (other_monkey_to_update, untouched_monkeys) = removeMonkeyWithIndex monkey_to_update_index other_monkeys
        let updated_other_monkey_to_update = pushItem other_monkey_to_update worry_level

        let new_monkey_list = [updated_monkey, updated_other_monkey_to_update] ++ untouched_monkeys
        runRoundHelper monkey_index new_monkey_list

-- Run arbitrary number of rounds of monkey turns
runRounds :: Int -> [Monkey] -> [Monkey]
runRounds 0 monkeys = monkeys
runRounds round monkeys = runRounds (round - 1) (runRound monkeys)

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

  -- part 2
  -- remove "divide by 3", instead modulo the worry level based on the LCM of the "divisible by" values

  -- tidy up
  hClose handle