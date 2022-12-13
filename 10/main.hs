import System.IO
import qualified Basement.String as S

data Op = Noop | Addx Int deriving(Show)

computeRegisterValue :: [Op] -> Int -> Int
computeRegisterValue _ 0 = 1
computeRegisterValue (Noop:ops) n = computeRegisterValue (ops) (n - 1)
computeRegisterValue ((Addx amount):ops) 1 = 1
computeRegisterValue ((Addx amount):ops) 2 = 1
computeRegisterValue ((Addx amount):ops) n = amount + computeRegisterValue ops (n - 2)

stringToOp :: String -> Op
stringToOp [] = Noop
stringToOp str
  | "noop" `elem` (words str) = Noop
  | "addx" `elem` (words str) = Addx ((read . last . words) str)
  | otherwise = error ("error: " ++ str)

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/10/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let all_lines = lines contents
  let ops = map stringToOp all_lines
  let cycles_to_report = [20 + x * 40 | x <- [0..5]]
  let results = map (\cycle -> cycle * computeRegisterValue ops cycle) cycles_to_report
  print (sum results)

  -- cleanup
  hClose handle