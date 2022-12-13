import System.IO

data TicTacToe = TicTacToe {
  board :: A.Array TileIndex TileState
}

main = do
  print "HEY"