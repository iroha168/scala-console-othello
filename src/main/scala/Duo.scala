case class Duo(val player:Player, val opponent: Player, val board: Board) {
  def move(coordinate: Coordinate, playerColoer: Cell,opponentrColoer: Cell): Board = {
    player.move(coordinate, playerColoer, opponentrColoer, board)
  }

  def swap: Duo = {
    this.copy(player = opponent, opponent = player)
  }


}
