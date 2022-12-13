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

breakLineByComma :: String -> [String]
breakLineByComma str = splitOn "," str


-- breakIntoTuples :: String -> (String, String)

main = do
  handle <- openFile "/Users/josephbowler/agora/aoc2022/04/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let assignmentTuples = map (map getScheduleFromString . breakLineByComma) (lines contents)
  let isOverlappingList = map (\[x, y] -> isOverlapping x y) assignmentTuples
  let overlappingTuples = filter id isOverlappingList
  print $ length overlappingTuples

  -- tie up loose ends
  hClose handle