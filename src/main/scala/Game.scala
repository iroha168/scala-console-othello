object Game {
  var blackPlayer: Player = Cpu()
  var whitePlayer: Player = Cpu()

  def start(): Unit = {
    val disp = new Display()
    var board = Board()
    var pass = 0
    var duo = Duo(board, blackPlayer, whitePlayer)
    while (true) {
      if (board.isNoChoice(duo.playerColor, duo.opponentColor, board)) pass += 1
      else {
        pass = 0
        val choice = duo.player.chooseMove(board, duo.playerColor, duo.opponentColor)
        println(choice)
        board = duo.player.move(choice, duo.playerColor, duo.opponentColor, board)
        disp.printBoard(board)
      }
      if (isFinished(pass, board)) return
      duo = turn(duo)
      Thread.sleep(1000)
    }
  }

  def turn(duo: Duo): Duo = {
    duo.swap
  }

  def isFinished(pass: Int, board: Board): Boolean = {
    if (pass >= 2) true
    else if (board.emptyCnt == 0) true
    else false
  }
}

