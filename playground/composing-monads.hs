import System.IO
import Data.List

readInput1 :: IO(Int)
readInput1 = do
  var <- getLine
  return (read var)

printOutput :: Int -> IO()
printOutput n = do
  putStrLn $ "Result: " ++ (show n)

list = reverse [1, 2, 3, 4, 5]

addIndices :: Int -> Int -> [Int] -> Maybe Int
addIndices x y arr = do
  first <- findIndex (\z -> z == x) arr
  second <- findIndex (\z -> z == y) arr

  return $ first + second

main = do
  x <- readInput1
  y <- readInput1

  let result = addIndices x y list
  case result of
    Just r -> printOutput r
    Nothing -> print "Failure"