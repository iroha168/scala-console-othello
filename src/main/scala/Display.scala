trait Display {
  def printBoard(board: Board): Unit
}

class ConsoleDisp extends Display {
  override def printBoard(board: Board): Unit = {
    print(board.show)
  }
}
