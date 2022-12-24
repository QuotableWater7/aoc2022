-- In progress...

import System.IO
import Data.List.Split
import Text.Regex.TDFA

-- Stack
data Stack a = Stack [a] deriving(Show)

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack xs) = Stack (x:xs)

popStack :: Stack a -> (a, Stack a)
popStack (Stack []) = error "empty stack"
popStack (Stack (x:xs)) = (x, Stack xs)

makeStackList :: Int -> [Stack Char]
makeStackList 1 = [Stack ""]
makeStackList n = makeStackList (n - 1) ++ [Stack ""]

-- for part 1

popStackAtIndex :: Int -> [Stack Char] -> (Char, [Stack Char])
popStackAtIndex n [] = error "No stacks"
popStackAtIndex i stacks = (char, updatedStacks)
  where
    char = (fst . popStack) (stacks!!(i - 1))
    updatedStacks = updateStackAtIndex i (\(Stack (x:xs)) -> Stack xs) stacks

pushStackAtIndex :: Int -> Char -> [Stack Char] -> [Stack Char]
pushStackAtIndex 1 char ((Stack xs):stacks) = Stack (char:xs) : stacks
pushStackAtIndex n char (s:stacks) = s : pushStackAtIndex (n - 1) char stacks
pushStackAtIndex n ch stacks = error $ "Error: reached " ++ (show n) ++ [ch]

-- for part 2
popStackAtIndexV2 :: Int -> Int -> [Stack Char] -> ([Char], [Stack Char])
popStackAtIndexV2 i amount stacks = popStacksAtIndexHelper i amount [] stacks
  where
    popStacksAtIndexHelper i 0 chars stacks = (chars, stacks)
    popStacksAtIndexHelper i n chars stacks = do
      let (char, updatedStacks) = popStackAtIndex i stacks
      popStacksAtIndexHelper i (n - 1) (chars ++ [char]) updatedStacks

pushStackAtIndexV2 :: Int -> [Char] -> [Stack Char] -> [Stack Char]
pushStackAtIndexV2 i chars stacks = do
  updateStackAtIndex i (\(Stack xs) -> Stack (chars ++ xs)) stacks


updateStackAtIndex :: Int -> (Stack a -> Stack a) -> [Stack a] -> [Stack a]
updateStackAtIndex _ _ [] = []
updateStackAtIndex 1 updateFn (s:stacks) = (updateFn s):stacks
updateStackAtIndex index updateFn (s:stacks) = s:(updateStackAtIndex (index - 1) updateFn stacks)

-- Move
data Move = Move { sourceIndex :: Int, destIndex :: Int, amount :: Int } deriving(Show)

parseMove :: String -> Move
parseMove str = Move {
  amount = read . last $ amount,
  sourceIndex = read . last $ sourceIndex,
  destIndex = read . last $ destIndex
}
  where
   [amount, sourceIndex, destIndex]= (str =~ "([0-9]+)"::[[String]])

-- Helpers
extractNumberLine :: [String] -> (String, [String])
extractNumberLine [] = error "Should be a number line!"
extractNumberLine (x:xs) = (x, xs)

countNumbersFromNumberLine :: String -> Int
countNumbersFromNumberLine str = length (str =~ "([0-9]+)"::[[String]])

processLine :: String -> [Stack Char] -> [Stack Char]
processLine [] stacks = stacks
processLine (_:' ':_:_:string) (s:stacks) = s : processLine string stacks
processLine (_:char:_:_:string) (s:stacks) = (pushStack char s) : processLine string stacks

readLinesIntoStacks :: [String] -> [Stack Char] -> [Stack Char]
readLinesIntoStacks [] stacks = stacks
readLinesIntoStacks (line:lines) stacks = readLinesIntoStacks lines $ processLine line stacks

initializeStacks :: String -> [Stack Char]
initializeStacks str = do
  let gameLines = reverse $ splitOn "\n" str

  let (numberLine, blockLists) = extractNumberLine gameLines
  let numStacksNeeded = countNumbersFromNumberLine numberLine

  let emptyStacks = makeStackList numStacksNeeded
  readLinesIntoStacks blockLists emptyStacks

initializeMoves :: String -> [Move]
initializeMoves str = do
  let lines = splitOn "\n" str
  map parseMove lines

applyMoveV1 :: Move -> [Stack Char] -> [Stack Char]
applyMoveV1 move stacks
  | amt == 0      = stacks
  | otherwise     = do
    let (char, updatedStacks) = popStackAtIndex src stacks
    let updatedStacks' = pushStackAtIndex dest char updatedStacks
    let next_move = move { amount = amt - 1 }
    applyMoveV1 next_move updatedStacks'
  where
    amt = amount move
    src = sourceIndex move
    dest = destIndex move

applyMoveV2 :: Move -> [Stack Char] -> [Stack Char]
applyMoveV2 move stacks
  | amt == 0      = stacks
  | otherwise     = do
    let (chars, updatedStacks) = popStackAtIndexV2 src amt stacks
    pushStackAtIndexV2 dest chars updatedStacks
  where
    amt = amount move
    src = sourceIndex move
    dest = destIndex move

applyMoves :: [Move] -> (Move -> [Stack Char] -> [Stack Char]) -> [Stack Char] -> [Stack Char]
applyMoves [] _ stacks = stacks
applyMoves (m:ms) applyMove stacks = applyMoves ms applyMove (applyMove m stacks)

main = do
  -- read input
  handle <- openFile "/Users/josephbowler/agora/aoc2022/05/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let [gameStateStr, movesStr] = splitOn "\n\n" contents

  let moves = initializeMoves movesStr
  let stacks = initializeStacks gameStateStr

  let updatedStacks = applyMoves moves applyMoveV1 stacks
  print updatedStacks

  -- part 2
  let updatedStacksPart2 = applyMoves moves applyMoveV2 stacks
  print updatedStacksPart2

  -- tidy up
  hClose handle