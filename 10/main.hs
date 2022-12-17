import System.IO
import qualified Basement.String as S

data Op = Noop | Addx Int deriving(Show)

computeRegisterValue :: [Op] -> Int -> Int
computeRegisterValue _ 0 = 1
computeRegisterValue (Noop:ops) n = computeRegisterValue (ops) (n - 1)
computeRegisterValue ((Addx amount):ops) 1 = 1
computeRegisterValue ((Addx amount):ops) 2 = 1
computeRegisterValue ((Addx amount):ops) n = amount + computeRegisterValue ops (n - 2)

convertToPixel :: [Op] -> Int -> Char
convertToPixel ops pixel_index = 
  if position_difference <= 1 
  then do '#' 
  else do '.'
  where
    register_value = computeRegisterValue ops pixel_index
    pixel_x_pos = (pixel_index - 1) `mod` 40
    position_difference = abs (register_value - pixel_x_pos)

stringToOp :: String -> Op
stringToOp [] = Noop
stringToOp str
  | "noop" `elem` (words str) = Noop
  | "addx" `elem` (words str) = Addx ((read . last . words) str)
  | otherwise = error ("error: " ++ str)

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n list = [n_elements] ++ chunk n rest
  where
    (n_elements, rest) = splitAt n list

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/10/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let all_lines = lines contents
  let ops = map stringToOp all_lines
  let cycles_to_report = [20 + x * 40 | x <- [0..5]]
  let results = map (\cycle -> cycle * computeRegisterValue ops cycle) cycles_to_report
  print (sum results)

  -- part 2
  let pixel_indices = [1..240]
  let pixels = map (convertToPixel ops) pixel_indices
  let pixelLines = chunk 40 pixels
  mapM print pixelLines

  -- cleanup
  hClose handle