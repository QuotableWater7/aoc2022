import System.IO

data TicTacToe = TicTacToe {
  board :: [[String]] TileIndex TileState
}

main = do
  print "HEY"