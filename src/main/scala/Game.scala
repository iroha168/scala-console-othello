object Game {
  var blackPlayer: Player = Cpu()
  var whitePlayer: Player = Cpu()

  def start(): Unit = {
    val disp = new Display()
    var board = Board()
    var pass = 0
    var duo = new Duo(blackPlayer, whitePlayer, board)
    while (true) {
      if (board.isNoChoice(color(duo.player), color(duo.opponent), board)) {
        pass += 1
      } else {
        val choice = duo.player.chooseMove(board, color(duo.player), color(duo.opponent))
        println(choice)
        board = duo.player.move(choice, color(duo.player), color(duo.opponent), board)
        disp.printBoard(board)
      }
      if (isFinished(pass, board)) {
        return
      }
      duo = turn(duo)
      Thread.sleep(500)
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

  def switch(target: Player): Player = {
    if (target == blackPlayer) whitePlayer
    else blackPlayer
  }

  def color(target: Player): Cell = {
    if (target == blackPlayer) Black
    else White
  }
}

