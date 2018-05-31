import scala.math._

class Players(var first: Player, var second: Player)

trait Player {
  def chooseMove(board: Board, player: Cell, opponent: Cell): Coordinate

  def move(coordinate: Coordinate, player: Cell, opponent: Cell, board: Board): Board
}

class Cpu() extends Player {
  override def chooseMove(board: Board, player: Cell, opponent: Cell): Coordinate = {
    val manipulator = new Manipulator(board)
    val availables = manipulator.getValidMoves(player, opponent, board)
    val choice = floor(random * (availables.length)).toInt
    availables(choice)
  }

  override def move(coordinate: Coordinate, player: Cell, opponent: Cell, board: Board): Board = {
    val manipulator = new Manipulator(board)
    if (manipulator.canMove(coordinate, player, opponent, board)) {
      manipulator.move(coordinate, player, opponent)
    } else {
      board
    }
  }
}

object Cpu {
  def apply(): Cpu = new Cpu()
}

