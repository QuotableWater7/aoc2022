module Menu2 where

import System.IO

menu2Choices = ["beep", "boop"]

menu2Loop = do
  putStrLn "Menu 2:"
  mapM putStrLn menu2Choices
  return ()