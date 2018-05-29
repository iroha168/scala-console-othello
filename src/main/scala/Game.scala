class Game {
  var player: Player = Cpu(First)
  var opponent: Player = Cpu(Second)

  def start(): Unit = {
    val disp = new Display()
    var board = Board()
    var pass = 0
    while (true) {
      if (board.isNoChoice(player, player.switch, board)) {
        pass += 1
      } else {
        val choice = player.chooseMove(board)
        println(choice)
        board = player.move(choice, board)
        disp.printBoard(board)
      }
      if(isFinished(pass, board)){
        return
      }
      turn()
      Thread.sleep(500)
    }
  }

  def turn(): Unit = {
    player = player.switch
    opponent = opponent.switch
  }

  def isFinished(pass: Int, board: Board): Boolean = {
    if(pass >= 2) true
    else if(board.emptyCnt==0) true
    else false
  }
}

