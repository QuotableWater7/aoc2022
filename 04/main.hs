import System.IO
import Data.List.Split

data Schedule = Schedule { start::Int, end::Int } deriving(Show)

isOverlapping :: Schedule -> Schedule -> Bool
isOverlapping s1 s2 = 
  (start(s1) <= start(s2) && end(s1) >= end(s2)) || 
  (start(s2) <= start(s1) && end(s2) >= end(s1))

getScheduleFromString :: String -> Schedule
getScheduleFromString [] = error "gah"
getScheduleFromString str = Schedule { start=read(x), end=read(y) }
  where
    [x,y] = splitOn "-" str

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/04/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let assignmentTuples = map (map getScheduleFromString . (splitOn ",")) (lines contents)
  let answerPart1 = foldr (\[x, y] -> (+ if isOverlapping x y then 1 else 0)) 0 assignmentTuples
  print answerPart1

  -- part 2

  -- tie up loose ends
  hClose handle