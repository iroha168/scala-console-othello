case class Board private(cellVec: Vector[Vector[Cell]]) {
  def show: String = cellVec.map(cells =>
    cells.map(" " + _.show + " ").mkString("")).mkString("\n") + "\n"

  def isNoChoice(player: Cell, opponent: Cell, board: Board): Boolean = {
    val manipulator = new Manipulator(board)
    manipulator.isNoChoice(player, opponent)
  }

  def emptyCnt(): Int = {
    var cnt = 0
    cellVec.foreach(row => row.foreach(cell => if (cell == Empty) cnt += 1))
    cnt
  }

}

object Board {
  def apply(): Board = {
    Board(Vector(
      Vector.fill(8)(Empty),
      Vector.fill(8)(Empty),
      Vector.fill(8)(Empty),
      Vector(Empty, Empty, Empty, Black, White, Empty, Empty, Empty),
      Vector(Empty, Empty, Empty, White, Black, Empty, Empty, Empty),
      Vector.fill(8)(Empty),
      Vector.fill(8)(Empty),
      Vector.fill(8)(Empty)
    ))
  }
}

