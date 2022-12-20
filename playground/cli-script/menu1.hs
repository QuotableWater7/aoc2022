module Menu1 where

import System.IO

menu1Choices = ["bingo", "bongo"]
  
menu1Loop = do
  putStrLn "Menu 1:"
  mapM putStrLn menu1Choices
  return ()