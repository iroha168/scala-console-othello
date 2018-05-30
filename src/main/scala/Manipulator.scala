class Manipulator(val board: Board) {
  def move(choice: Coordinate, player: Cell, opponent: Cell): Board = {
    var opponentStones = Seq[Coordinate]()
    board.cellVec(choice.y).updated(choice.x, player)

    def reverse(d: Coordinate => Option[Coordinate], coordinate: Coordinate, board: Board): Board = {
      d(coordinate) match {
        case Some(coord) => {
          if (board.cellVec(coord.y)(coord.x) == opponent) {
            opponentStones = coord +: opponentStones
            reverse(d, coord, board)
          }
          else if (board.cellVec(coord.y)(coord.x) == player) {
            opponentStones.foldLeft(board)((transitBoard, stone) =>
              Board(board.cellVec.updated(stone.y, transitBoard.cellVec(stone.y).updated(stone.x, player))))
          }
          else {
            opponentStones = Nil
            board
          }
        }
        case None => board
      }
    }

    val reversedBoard = Direction.values.foldLeft(board)((brd, d) => {
      d(choice) match {
        case Some(coord) => {
          if (board.cellVec(coord.y)(coord.x) == opponent) reverse(d, choice, board)
          else brd
        }
        case None => brd
      }
    })
    Board(reversedBoard.cellVec.updated(choice.y, reversedBoard.cellVec(choice.y).updated(choice.x, player)))
  }

  def canMove(coordinate: Coordinate, player: Cell, opponent: Cell, board: Board): Boolean = {
    def isValidLine(d: Coordinate => Option[Coordinate], coord: Coordinate): Boolean = {
      d(coord) match {
        case Some(coord) => {
          if (board.cellVec(coord.y)(coord.x) == opponent) {
            isValidLine(d, coord)
          }
          if (board.cellVec(coord.y)(coord.x) == player) true
          else false
        }
        case None => false
      }
    }

    if (board.cellVec(coordinate.y)(coordinate.x) != Empty) false
    else {
      Direction.values.exists(d => {
        d(coordinate) match {
          case Some(coord) => {
            if (board.cellVec(coord.y)(coord.x) == opponent) isValidLine(d, coord)
            else false
          }
          case None => false
        }
      })
    }
  }

  def isNoChoice(player: Cell, opponent: Cell): Boolean =
    !(0 until 8).exists(y => (0 until 8).exists(x => canMove(new Coordinate(y, x), player, opponent, board)))

  def getValidMoves(player: Cell, opponent: Cell, board: Board): Seq[Coordinate] = {
    for {
      y <- 0 until 8
      x <- 0 until 8
      coord = new Coordinate(y, x)
      if (canMove(coord, player, opponent, board))
    } yield coord
  }
}

