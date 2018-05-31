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

  def updateCellColor(coordinate: Coordinate, color: Cell) = {
    Board(this.cellVec.updated(coordinate.y, this.cellVec(coordinate.y).updated(coordinate.x, color)))
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

