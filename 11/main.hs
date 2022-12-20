import System.IO
import Text.Regex.TDFA
import Data.List
import Data.List.Split
import Data.Function

data Variable = DynamicValue | HardcodedValue Int deriving(Show)
data OpType = Multiply | Add
data Operation = Operation OpType Variable Variable
data Expression = Expression OpType Int Int

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

data Monkey = Monkey {
  index :: Int,
  items :: [Int],
  operation :: Operation,
  test :: Int -> Bool,
  testTrue :: Int,
  testFalse :: Int
}

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

instance Show Monkey where
  show m = show (index m) ++ "/" ++ show (items m) ++ "/" ++ show (testTrue m) ++ "/" ++ show (testFalse m)

runTurnForMonkey :: Int -> [Monkey] -> Maybe ([Monkey], Int)
runTurnForMonkey monkeyIndex monkeys = do
  -- current monkey
  currentMonkey <- find (\m -> index m == monkeyIndex) monkeys
  let (item, updatedCurrentMonkey) = removeFirstItem currentMonkey
  let worryLevel = computeWorryLevel currentMonkey item

  -- monkey to update
  let monkeyToUpdateIndex = if test currentMonkey worryLevel then testTrue currentMonkey else testFalse currentMonkey
  monkeyToUpdate <- find (\m -> index m == monkeyToUpdateIndex) monkeys
  let updatedMonkeyToUpdate = pushItem monkeyToUpdate worryLevel

  -- prepare results
  let otherMonkeys = filter (\m -> not ((index m) `elem` [monkeyIndex, monkeyToUpdateIndex])) monkeys
  let updatedMonkeyList = [updatedCurrentMonkey, updatedMonkeyToUpdate] ++ otherMonkeys
  let nextMonkeyIndex = if items updatedCurrentMonkey == [] then if monkeyIndex + 1 < length monkeys then monkeyIndex + 1 else -1 else monkeyIndex
  
  return (updatedMonkeyList, nextMonkeyIndex)

-- turnLoop :: Int -> [Monkey] -> IO()
-- turnLoop monkeyIndex monkeys = do
--   (updatedMonkeys, nextIndex) <- runTurnForMonkey monkeyIndex monkeys
--   print (show nextIndex)
--   return ()
--   -- return (if not (nextIndex == -1)
--   --   then turnLoop nextIndex updatedMonkeys
--   --   else updatedMonkeys)

parseMonkey :: String -> Monkey
parseMonkey str = Monkey { index=index, items=items, operation=operation, test=test, testTrue=testTrue, testFalse=testFalse }
  where
    index = ((read :: String -> Int) . last . head) (str =~ "Monkey ([0-9]+):"::[[String]])
    items = ((map (read :: String->Int)) . (splitOn ", ") . last . head) (str =~ "Starting items: (.*)"::[[String]])
    opType = (last . head) (str =~ "Operation:.*([+*]).*"::[[String]])
    var1 = (last . head) (str =~ "Operation:.*([a-z0-9]+) [+*].*"::[[String]])
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
  -- let updatedMonkeys = turnLoop 0 monkeys
  let result = runTurnForMonkey 0 monkeys

  -- case result of 
  --   (Just (updatedMonkeys, nextIndex)) -> mapM print updatedMonkeys
  --   Nothing -> mapM print "OOPS"
  
  -- mapM print updatedMonkeys
  -- let first_monkey = head monkeys
  -- let op = convertOperationToExpression (operation first_monkey) 79
  -- print op
  -- print (computeWorryLevel (head monkeys) 79)

  -- tidy up
  hClose handle