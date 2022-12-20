import System.IO
import Menu1 (menu1Loop)
import Menu2 (menu2Loop)

valid_choices = [menu1Loop, menu2Loop]

main = do
  putStrLn $ "Enter a choice from 1-" ++ (show . length $ valid_choices) ++ ":"
  c <- getLine
  putStrLn ""
  
  if c == "1" then menu1Loop
  else if c == "2" then menu2Loop
  else do
    putStrLn "Invalid option, please try again\n"

  putStrLn ""
  main