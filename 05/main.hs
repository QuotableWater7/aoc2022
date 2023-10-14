-- In progress...

import System.IO
import Data.List.Split
import Text.Regex.TDFA
import Text.Read (readEither)

-- Stack
-- The data structure is generic but most of the supporting functions
-- for AoC depend specifically on Char/String behavior
newtype Stack a = Stack [a] deriving(Show)

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack xs) = Stack (x:xs)

popStack :: Stack a -> (a, Stack a)
popStack (Stack []) = error "empty stack"
popStack (Stack (x:xs)) = (x, Stack xs)

-- Part 1

-- There is a line of input with n numbers. This func initializes n empty stacks
makeStackList :: Int -> [Stack Char]
makeStackList 1 = [Stack ""]
makeStackList n = makeStackList (n - 1) ++ [Stack ""]

-- Like popStack, except pops from a specific stack among a list of stacks
popStackAtIndex :: Int -> [Stack Char] -> (Char, [Stack Char])
popStackAtIndex n [] = error "No stacks"
popStackAtIndex i stacks = (char, updatedStacks)
  where
    char = (fst . popStack) (stacks!!(i - 1))
    updatedStacks = updateStackAtIndex i (\(Stack (x:xs)) -> Stack xs) stacks

-- Like pushStack, except pops from a specific stack among a list of stacks
pushStackAtIndex :: Int -> Char -> [Stack Char] -> [Stack Char]
pushStackAtIndex 1 char ((Stack xs):stacks) = Stack (char:xs) : stacks
pushStackAtIndex n char (s:stacks) = s : pushStackAtIndex (n - 1) char stacks
pushStackAtIndex n ch stacks = error $ "Error: reached " ++ show n ++ [ch]

-- Part 2 --

-- Like popStackAtIndex, except we need to pop multiple items at once in the
-- same order as they already appear on the stack.
popStackAtIndexV2 :: Int -> Int -> [Stack Char] -> ([Char], [Stack Char])
popStackAtIndexV2 i amount stacks = popStacksAtIndexHelper i amount [] stacks
  where
    popStacksAtIndexHelper i 0 chars stacks = (chars, stacks)
    popStacksAtIndexHelper i n chars stacks = do
      let (char, updatedStacks) = popStackAtIndex i stacks
      popStacksAtIndexHelper i (n - 1) (chars ++ [char]) updatedStacks

-- Like pushStackAtIndex, except we need to push multiple items at once.
pushStackAtIndexV2 :: Int -> [Char] -> [Stack Char] -> [Stack Char]
pushStackAtIndexV2 i chars stacks = do
  updateStackAtIndex i (\(Stack xs) -> Stack (chars ++ xs)) stacks

-- Apply an arbitrary update to a stack at a particular index.
updateStackAtIndex :: Int -> (Stack a -> Stack a) -> [Stack a] -> [Stack a]
updateStackAtIndex _ _ [] = []
updateStackAtIndex 1 updateFn (s:stacks) = updateFn s : stacks
updateStackAtIndex index updateFn (s:stacks) = s : updateStackAtIndex (index - 1) updateFn stacks

-- Move: store details about how we will move blocks around
data Move = Move { sourceIndex :: Int, destIndex :: Int, amount :: Int } deriving(Show)

parseMove :: String -> Either String Move
parseMove str = do
  let parseResults = str =~ "([0-9]+)"::[[String]]

  if length parseResults /= 3
    then Left $ "Invalid move: " ++ str
    else do
      let [amount, sourceIndex, destIndex] = parseResults

      Right Move {
        amount = read . last $ amount,
        sourceIndex = read . last $ sourceIndex,
        destIndex = read . last $ destIndex
      }

-- Helpers
countNumbersFromNumberLine :: String -> Int
countNumbersFromNumberLine str = length (str =~ "([0-9]+)"::[[String]])

processLine :: String -> [Stack Char] -> [Stack Char]
processLine [] stacks = stacks
processLine (_:' ':_:_:string) (s:stacks) = s : processLine string stacks
processLine (_:char:_:_:string) (s:stacks) = pushStack char s : processLine string stacks

readLinesIntoStacks :: [String] -> [Stack Char] -> [Stack Char]
readLinesIntoStacks lines stacks = foldl (flip processLine) stacks lines

initializeStacks :: String -> [Stack Char]
initializeStacks str = do
  let (numberLine:blockLists) = reverse $ splitOn "\n" str
  let numStacksNeeded = countNumbersFromNumberLine numberLine

  let emptyStacks = makeStackList numStacksNeeded
  readLinesIntoStacks blockLists emptyStacks

-- Parse a string containing all the moves into a Move list
initializeMoves :: String -> Either String [Move]
initializeMoves str = do
  let lines = splitOn "\n" str
  mapM parseMove lines

-- For every move, we pop a stack at one index and then push onto a stack.
-- If "amount" is greater than 1, we recursively call applyMoveV1 and decrement
-- amount until amount is 0 and we return our results.
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

-- Similar to applyMoveV1 but we pull multiple chars at a time instead of just 1.
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

-- Given a list of moves and a function to transform stacks from a single move,
-- we can apply all moves to the stacks.
applyMoves :: [Move] -> (Move -> [Stack Char] -> [Stack Char]) -> [Stack Char] -> [Stack Char]
applyMoves [] _ stacks = stacks
applyMoves (m:ms) applyMove stacks = applyMoves ms applyMove (applyMove m stacks)

-- Helper to print the result of each problem part
getHeadOfStacks :: [Stack Char] -> String
getHeadOfStacks [] = []
getHeadOfStacks (x:xs) = do
  let (char, updatedStack) = popStack x
  char : getHeadOfStacks xs

main = do
  -- read input
  handle <- openFile "05/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let [gameStateStr, movesStr] = splitOn "\n\n" contents

  let moves = initializeMoves movesStr

  case moves of
    Left error -> print $ "Failed to generate moves: " ++ error
    Right moves -> do
      let stacks = initializeStacks gameStateStr

      let updatedStacksPart1 = applyMoves moves applyMoveV1 stacks
      print $ getHeadOfStacks updatedStacksPart1

      -- part 2
      let updatedStacksPart2 = applyMoves moves applyMoveV2 stacks
      print $ getHeadOfStacks updatedStacksPart2

  -- tidy up
  hClose handle