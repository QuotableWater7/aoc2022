import System.IO
import Text.Regex.TDFA

data FileOrFolder = File Int | Folder [FileOrFolder] deriving(Show)

data TerminalLine = CD String | LS | StdOut String deriving(Show)

parseTerminalLine :: String -> Either String TerminalLine
parseTerminalLine "" = Left "Cannot parse empty string into TerminalLine"
parseTerminalLine s
  | (not . null) cdRegexResults     = Right $ CD cdValue
  | isLS                            = Right LS
  | (not . null) stdOut             = Right $ StdOut stdOut
  | otherwise                       = Left $ "Error parsing terminal line: " ++ s
  where
    cdRegexResults =  s =~ "\\$ cd *(.*)" :: [[String]]
    cdValue = last . head $ cdRegexResults
    isLS = (not . null) $ (s =~ "$ ls" :: [[String]])
    stdOut = last . head $ (s =~ "[^$].*" :: [[String]])

main = do
  -- read file contents
  handle <- openFile "07/input.txt" ReadMode
  contents <- hGetContents handle

  let terminalLines = mapM parseTerminalLine (lines contents)
  case terminalLines of
    Left error -> print $ "Error: " ++ error
    Right terminalLines -> print terminalLines

  hClose handle