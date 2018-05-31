object Direction {
  val values: List[Coordinate => Option[Coordinate]] = {
    def upper(coordinate: Coordinate): Option[Coordinate] = next(coordinate, -1, 0)

    def upperRight(coordinate: Coordinate): Option[Coordinate] = next(coordinate, -1, 1)

    def upperLeft(coordinate: Coordinate): Option[Coordinate] = next(coordinate, -1, -1)

    def right(coordinate: Coordinate): Option[Coordinate] = next(coordinate, 0, 1)

    def left(coordinate: Coordinate): Option[Coordinate] = next(coordinate, 0, -1)

    def bottom(coordinate: Coordinate): Option[Coordinate] = next(coordinate, 1, 0)

    def bottomRight(coordinate: Coordinate): Option[Coordinate] = next(coordinate, 1, 1)

    def bottomLeft(coordinate: Coordinate): Option[Coordinate] = next(coordinate, 1, -1)

    def next(coordinate: Coordinate, directionY: Int, directionX: Int): Option[Coordinate] = {
      val y = coordinate.y + directionY
      val x = coordinate.x + directionX
      if (y < 0 || x < 0 || y > 7 || x > 7) None
      else Option(new Coordinate(y, x))
    }

    List(upper, upperRight, upperLeft, right, bottom, bottomRight, bottomLeft, left)
  }
}
