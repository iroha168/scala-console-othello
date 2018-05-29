import scala.math._

class Players(var first: Player, var second: Player)

trait Player {
  val color: Cell

  def chooseMove(board: Board): Coordinate

  def switch: Player

  def move(coordinate: Coordinate, board: Board): Board
}

class Cpu(val color: Cell) extends Player {
  override def chooseMove(board: Board): Coordinate = {
    val manipulator = new Manipulator(board)
    val availables = manipulator.getValidMoves(this, this.switch, board)
    val choice = floor(random * (availables.length)).toInt
    availables(choice)
  }

  override def switch: Player = {
    if (this.color == Black) Cpu(Second)
    else Cpu(First)
  }

  override def move(coordinate: Coordinate, board: Board): Board = {
    val manipulator = new Manipulator(board)
    if (manipulator.canMove(coordinate, this, this.switch, board)) {
      manipulator.move(coordinate, this, this.switch)

    } else {
      board
    }
  }
}

object Cpu {
  def apply(order: Order): Cpu = new Cpu(
    order.color
  )
}

sealed trait Order {
  val color: Cell
}

case object First extends Order {
  override val color = Black
}

case object Second extends Order {
  override val color = White
}

