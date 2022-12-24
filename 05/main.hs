-- In progress...

import System.IO
import Data.List.Split
import Text.Regex.TDFA

data Stack a = Stack [a]

emptyStack :: Stack a
emptyStack = Stack []

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack xs) = Stack (x:xs)

popStack :: Stack a -> (a, Stack a)
popStack (Stack []) = error "empty stack"
popStack (Stack (x:xs)) = (x, Stack xs)

makeStackList :: Int -> [Stack a]
makeStackList 1 = [emptyStack]
makeStackList n = makeStackList (n - 1) ++ [emptyStack]

updateStackAtIndex :: Int -> (Stack a -> Stack a) -> [Stack a] -> [Stack a]
updateStackAtIndex _ _ [] = []
updateStackAtIndex 0 updateFn (s:stacks) = (updateFn s):stacks
updateStackAtIndex index updateFn (s:stacks) = s:(updateStackAtIndex (index - 1) updateFn stacks)

data GameBoard a = GameBoard { stacks :: [Stack a] }

parseGameBoard :: String -> GameBoard Int
parseGameBoard str = do
  GameBoard {
    stacks=[]
  }

addLineToBoard :: String -> GameBoard a -> GameBoard a
addLineToBoard [] gameboard = gameboard
addLineToBoard list gameboard = gameboard
  

buildGameboard :: Int -> [String] -> GameBoard Int
buildGameboard size gameLines = do
  let emptyGameBoard = GameBoard {
    stacks = makeStackList size
  }
  emptyGameBoard


data Move = Move { sourceIndex :: Int, destIndex :: Int, amount :: Int } deriving(Show)

extractNumberLine :: [String] -> (String, [String])
extractNumberLine [] = error "Should be a number line!"
extractNumberLine (x:xs) = (x, xs)

countNumbersFromNumberLine :: String -> Int
countNumbersFromNumberLine str = length (str =~ "([0-9]+)"::[[String]])


-- parseGameState :: String -> (GameBoard, Moves)
parseGameState :: String -> ([String], [String])
parseGameState str = do
  let [gameStateStr, movesStr] = splitOn "\n\n" str
  let gameLines = reverse $ splitOn "\n" gameStateStr

  let (numberLine, blockLists) = extractNumberLine gameLines
  let numStacksNeeded = countNumbersFromNumberLine numberLine
  let emptyStacks = makeStackList numStacksNeeded

  ([show numStacksNeeded], blockLists)


main = do
  -- read input
  handle <- openFile "/Users/josephbowler/agora/aoc2022/05/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let parsed = parseGameState contents
  print parsed

  -- part 2
  -- remove "divide by 3", instead modulo the worry level based on the LCM of the "divisible by" values

  -- tidy up
  hClose handle