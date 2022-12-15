import System.IO
import Text.Regex.TDFA
import Data.List.Split

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
  startingItems :: [Int],
  operation :: Operation,
  test :: Int -> Bool,
  testTrue :: Int,
  testFalse :: Int
}

instance Show Monkey where
  show m = show (index m) ++ "/" ++ show (startingItems m) ++ "/" ++ show (testTrue m) ++ "/" ++ show (testFalse m)

parseMonkey :: String -> Monkey
parseMonkey str = Monkey { index=index, startingItems=startingItems, operation=operation, test=test, testTrue=testTrue, testFalse=testFalse }
  where
    index = ((read :: String -> Int) . last . head) (str =~ "Monkey ([0-9]+):"::[[String]])
    startingItems = ((map (read :: String->Int)) . (splitOn ", ") . last . head) (str =~ "Starting items: (.*)"::[[String]])
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
  print monkeys

  -- tidy up
  hClose handle