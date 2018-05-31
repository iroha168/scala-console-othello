case class Duo(val board: Board, val player: Player, val opponent: Player, val blackPlayer: Player, val whitePlayer: Player) {
  def move(coordinate: Coordinate, playerColoer: Cell, opponentrColoer: Cell): Board = {
    player.move(coordinate, playerColoer, opponentrColoer, board)
  }


  def swap: Duo = {
    this.copy(player = opponent, opponent = player)
  }

  def switch(target: Player): Duo = {
    this.copy(blackPlayer = opponent, whitePlayer = player)
  }

  def playerColor: Cell = {
    if (player == blackPlayer) Black
    else White
  }

  def opponentColor: Cell = {
    if (opponent == blackPlayer) Black
    else White
  }
}

object Duo {
  def apply(board: Board, black: Player, white: Player): Duo = {
    new Duo(board, black, white, black, white)
  }
}
