import scala.math._

class Game {
  val WALL = -1
  val EMPTY = 0
  val BLACK = 1
  val WHITE = 2
  val AVAILABLE = 3
  val EMPTY_SYMBOL = " . "
  val BLACK_SYMBOL = " X "
  val WHITE_SYMBOL = " 0 "
  val AVAILABLE_SYMBOL = " * "
  val cells = Array.ofDim[Int](10, 10)
  var availables: List[Coordinate] = Nil
  var player: Int = BLACK

  //range operation on all cells
  private def processAll(f: Coordinate => Unit, rowCap: Int = 10, colCap: Int = 10) = {
    (0 until rowCap).foreach(row => (0 until colCap).foreach(col => f(new Coordinate(row, col))))
  }

  private def getAvailableMoves(): List[Coordinate] = {
    (1 until 9).flatMap(row => (1 until 9).flatMap(col => (-1 to 1).flatMap(directionY => (-1 to 1).
      flatMap(directionX => tryToMoveInADirection(row)(col)(directionY, directionX))))).toList
  }

  private def init(): Unit = {
    def initCell(coordinate: Coordinate): Unit = {
      cells(coordinate.y)(coordinate.x) = (coordinate.y, coordinate.x) match {
        case (0, _) => WALL
        case (_, 0) => WALL
        case (9, _) => WALL
        case (_, 9) => WALL
        case (4, 5) => BLACK
        case (5, 4) => BLACK
        case (4, 4) => WHITE
        case (5, 5) => WHITE
        case _ => EMPTY
      }
    }

    processAll(initCell _)
  }

  private def showBoard(): Unit = {
    def showCell(coordinate: Coordinate): Unit = {
      (coordinate.y, coordinate.x) match {
        case (_, 0) => print("\n" + getSymbol(coordinate))
        case _ => print(getSymbol(coordinate))
      }
    }

    processAll(showCell _)
  }

  private def getSymbol(coordinate: Coordinate): String = {
    cells(coordinate.y)(coordinate.x) match {
      case BLACK => BLACK_SYMBOL
      case WHITE => WHITE_SYMBOL
      case EMPTY => EMPTY_SYMBOL
      case AVAILABLE => AVAILABLE_SYMBOL
      case _ => ""
    }
  }

  //activate and commence a game
  def run() = {
    init()
    setAvailableMoves()
    play()
  }

  private def play() = {
    val choice = choose()
    showBoard()
  }

  private def isFinished: Boolean = {
    return false
  }

  //update the available list for the current player
  private def setAvailableMoves() = {
    def setAvailablePosOnBoard(coordinate: Coordinate) = {
      availables = coordinate :: availables
      cells(coordinate.y)(coordinate.x) = AVAILABLE
    }

    availables = Nil
    val moves = getAvailableMoves()
    moves.map(move => setAvailablePosOnBoard(move))
  }

  //choose next move among valid ones
  private def choose(): Option[Coordinate] = {
    if (availables.length > 0) {
      val choice = floor(random * (availables.length - 1)).toInt
      Option(availables(choice))
    }
    else None
  }

  //make a move. convert the board status to the one after the move
  private def move(coordinate: Coordinate) = {
    ???
  }

  //get the current opponent
  private def opponent: Int = {
    3 - player
  }

  //check weather or not the specified move is valid.
  //the move is specified by the cell on which a stone is put
  //and a direction
  private def tryToMoveInADirection(row: Int)(col: Int)(offsetY: Int, offsetX: Int): Option[Coordinate]

  = {
    def proceed(y: Int, x: Int): (Int, Int) = {
      val nextRow = y + offsetY
      val nextCol = x + offsetX
      cells(nextRow)(nextCol) match {
        case x if x == opponent => proceed(nextRow, nextCol)
        case player => (nextRow, nextCol)
        case _ => (y, x)
      }
    }

    if (cells(row)(col) != EMPTY) None
    else if (offsetY == 0 && offsetX == 0) None
    else {
      if (cells(row + offsetY)(col + offsetX) != opponent) None
      else {
        val (destinationRow, destinationCol) = proceed(row, col)
        if (cells(destinationRow)(destinationCol) == player)
          Some(new Coordinate(row, col))
        else None
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game()
    game.run()
  }
}