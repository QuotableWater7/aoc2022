import System.IO
import Text.Regex.TDFA
import Data.List
import Data.List.Split
import Data.Function

-- basic types
data Variable = DynamicValue | HardcodedValue Int deriving(Show)
data OpType = Multiply | Add deriving(Show)

-- operation
data Operation = Operation OpType Variable Variable

instance Show Operation where
  show (Operation opType var1 var2) = "Op: " ++ (show opType) ++ " (" ++ (show var1) ++ ", " ++ (show var2) ++ ")"
  
-- expression
data Expression = Expression OpType Int Int

instance Show Expression where
  show (Expression opType value1 value2) = "Exp: " ++ (show opType) ++ " (" ++ (show value1) ++ ", " ++ (show value2) ++ ")"

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

-- monkey
data Monkey = Monkey {
  index :: Int,
  items :: [Int],
  operation :: Operation,
  test :: Int -> Bool,
  testTrue :: Int,
  testFalse :: Int
}

instance Show Monkey where
  show m = "Monkey: \n" ++ "\tIndex:\t" ++ show (index m) ++ "\n\tItems:\t" ++ show (items m) ++ "\n\t" ++ (show . operation $ m) ++ "\n\tTest True:\t" ++ show (testTrue m) ++ "\n\tTest False:\t" ++ show (testFalse m) ++ "\n"

-- helpers

makeOperation :: String -> String -> String -> Operation
makeOperation opStr str1 str2 = Operation opType var1 var2
  where
    opType = if opStr == "+" then Add else Multiply
    var1 = if str1 == "old" then DynamicValue else HardcodedValue ((read str2)::Int)
    var2 = if str2 == "old" then DynamicValue else HardcodedValue ((read str2)::Int)

hydrateVariable :: Variable -> Int -> Int
hydrateVariable (HardcodedValue hv) _ = hv
hydrateVariable (DynamicValue) value = value

convertOperationToExpression :: Operation -> Int -> Expression
convertOperationToExpression (Operation opType var1 var2) value = Expression opType (hydrateVariable var1 value) (hydrateVariable var2 value)

processExpression :: Expression -> Int
processExpression (Expression Add x y) = x + y
processExpression (Expression Multiply x y) = x * y

removeFirstItem :: Monkey -> (Int, Monkey)
removeFirstItem monkey = (item, updatedMonkey)
  where
    item = (head . items) monkey
    updatedMonkey = Monkey { 
      index=index monkey, 
      items=(tail . items) monkey,  
      operation=operation monkey,
      test=test monkey,
      testTrue=testTrue monkey,
      testFalse=testFalse monkey
    }

pushItem :: Monkey -> Int -> Monkey
pushItem monkey item = Monkey { 
  index=index monkey, 
  items=(items monkey) ++ [item],  
  operation=operation monkey,
  test=test monkey,
  testTrue=testTrue monkey,
  testFalse=testFalse monkey
}

computeWorryLevel :: Monkey -> Int -> Int
computeWorryLevel monkey item = result
  where
    worryLevelAfterInspection = processExpression $ convertOperationToExpression (operation monkey) item
    result = floor ((fromIntegral worryLevelAfterInspection) / 3)

runTest :: Monkey -> Int -> Int
runTest monkey worry_level = do
  case test monkey $ worry_level of
    True -> testTrue monkey
    False -> testFalse monkey

removeMonkeyAtIndex :: Int -> [Monkey] -> (Monkey, [Monkey])
removeMonkeyAtIndex i [] = error ("Did not find monkey at index " ++ (show i))
removeMonkeyAtIndex i monkeys = 
  case find ((== i) . index) monkeys of 
    Nothing -> error "no monke"
    Just m -> (m, filter (not . (== i) . index) monkeys)
  
runTurnForMonkeys :: Int -> [Monkey] -> [Monkey]
runTurnForMonkeys i monkeys
  | i == (length monkeys)           = monkeys
  | items monkey_to_update == []    = runTurnForMonkeys (i + 1) monkeys
  | otherwise                       = do
    let (item, updated_monkey) = removeFirstItem monkey_to_update
    let worry_level = computeWorryLevel monkey_to_update item

    let monkey_to_update_index = runTest monkey_to_update worry_level
    let (other_monkey_to_update, untouched_monkeys) = removeMonkeyAtIndex monkey_to_update_index other_monkeys
    let updated_other_monkey_to_update = pushItem other_monkey_to_update worry_level
    runTurnForMonkeys i ([updated_monkey, updated_other_monkey_to_update] ++ untouched_monkeys)
  where
    (monkey_to_update, other_monkeys) = removeMonkeyAtIndex i monkeys

runRoundsForMonkeys :: Int -> [Monkey] -> [Monkey]
runRoundsForMonkeys 0 monkeys = monkeys
runRoundsForMonkeys round monkeys = runRoundsForMonkeys (round - 1) (runTurnForMonkeys 0 monkeys)

parseMonkey :: String -> Monkey
parseMonkey str = Monkey { index=index, items=items, operation=operation, test=test, testTrue=testTrue, testFalse=testFalse }
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

main = do
  -- read input
  handle <- openFile "/Users/josephbowler/agora/aoc2022/11/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rawMonkeyStrings = splitOn ("\n\n") contents
  let monkeys = map parseMonkey rawMonkeyStrings
  let updated_monkeys = runRoundsForMonkeys 20 monkeys
  mapM print updated_monkeys

  -- debugging
  -- let result = runTurnForMonkey 0 monkeys
  -- case result of 
  --   (Just (updatedMonkeys, nextIndex)) -> do
  --     print $ show nextIndex
  --     -- mapM print updatedMonkeys
  --   Nothing -> print "YO"

  -- tidy up
  hClose handle