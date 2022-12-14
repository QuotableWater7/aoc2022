import System.IO
import Text.Regex.TDFA
import Data.List.Split

data Variable = DynamicValue | HardcodedValue Int deriving(Show)
data OpType = Multiply | Add
data Operation = Operation OpType Variable Variable

processOperation :: Operation -> Int -> Int
processOperation (Operation Add x y) variable = 

data Monkey = Monkey {
  index :: Int,
  startingItems :: [Int],
  operation :: Int -> Int,
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
    operation = (+ 4)
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