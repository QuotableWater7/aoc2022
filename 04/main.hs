import System.IO
import Data.List.Split
import Text.Regex.TDFA

data Schedule = Schedule { start::Int, end::Int } deriving(Show)

isFullyOverlapping :: Schedule -> Schedule -> Bool
isFullyOverlapping s1 s2 =
  (start s1 <= start s2 && end s1 >= end s2) || 
  (start s2 <= start s1 && end s2 >= end s1)

isPartiallyOverlapping :: Schedule -> Schedule -> Bool
isPartiallyOverlapping s1 s2 = 
  (start s1 <= start s2 && end s1 >= start s2) ||
  (start s2 < start s1 && end s2 >= start s1)

validateInt :: String -> Either String Int
validateInt "" = Left "Int must be non-empty"
validateInt coord = if (not . null) results
  then Right $ read ((head . head) results)
  else Left $ "Invalid int: " ++ coord
  where
    results = coord =~ "[0-9]+"::[[String]]

getScheduleFromString :: String -> Either String Schedule
getScheduleFromString "" = Left "Cannot make schedule from empty string"
getScheduleFromString str = do
  if length coords == 2 
    then do
      validated_x <- validateInt (head coords)
      validated_y <- validateInt (last coords)
      Right Schedule { start=validated_x, end=validated_y }
    else
      Left $ "Invalid schedule string provided: " ++ str
  where
    coords = splitOn "-" str

validateSchedule :: String -> Either String Schedule
validateSchedule str = do
  let coords = splitOn "-" str
  if length coords == 2
    then do
      validated_start <- validateInt (head coords)
      validated_end <- validateInt (last coords)
      Right $ Schedule { start=validated_start, end=validated_end }
    else Left $ "Need 2 coordinates to be provided: " ++ head coords

convertLineToSchedules :: String -> Either String (Schedule, Schedule)
convertLineToSchedules "" = Left "Cannot convert empty line to a pair of schedules"
convertLineToSchedules line = do
  let potential_schedules = splitOn "," line
  if length potential_schedules == 2
    then do
      first_schedule <- validateSchedule (head potential_schedules)
      second_schedule <- validateSchedule (last potential_schedules)
      Right (first_schedule, second_schedule)
    else
      Left $ "Invalid schedules provided: " ++ line


main = do
  handle <- openFile "04/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let all_lines = lines contents
  let all_schedules = mapM convertLineToSchedules all_lines
  case all_schedules of
    Left error -> print $ "Error Part 1: " ++ error
    Right all_schedules -> do
      let answerPart1 = length . filter (uncurry isFullyOverlapping) $ all_schedules
      print answerPart1

  -- part 2
  case all_schedules of
    Left error -> print $ "Error Part 2: " ++ error
    Right all_schedules -> do
      let answerPart2 = length . filter (uncurry isPartiallyOverlapping) $ all_schedules
      print answerPart2

  -- tie up loose ends
  hClose handle