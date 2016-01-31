import Color.Color
import scala.annotation.tailrec

object Color {
  type Color = String

  val Black = "X"
  val White = "O"
  val Empty = "."
}

case class Cell(x: Int, y: Int)

case class Connect(board: List[String]) {

  lazy val whiteBoard = board.map(l => l.filter(!_.isSpaceChar))
  lazy val blackBoard = whiteBoard.transpose.map(_.mkString)

  def result(): Option[Color] = {
    if (existsWhitePath) Some(Color.White)
    else if (existsBlackPath) Some(Color.Black)
    else None
  }

  private def existsWhitePath: Boolean = existsPath(whiteBoard, Color.White)

  private def existsBlackPath: Boolean = existsPath(blackBoard, Color.Black)

  private def createColorBoard(board: List[String], color: Color): Set[Cell] = {
    (for {
      row <- board.indices
      col <- board(row).indices
      if board(row)(col).toString == color
    } yield Cell(row, col)).toSet
  }

  private def existsPath(board: List[String], color: Color): Boolean = {

    val colorBoard = createColorBoard(board, color)
    val firstRow = colorBoard.filter(_.x == 0)
    val lastRow = colorBoard.filter(_.x == board.size - 1)

    @tailrec
    def existsAnyPath(visiting: Set[Cell], visited: Set[Cell]): Boolean = {
      if (visiting exists lastRow) true
      else if (visiting.isEmpty) false
      else {
        val newVisited = visited ++ visiting
        val newToVisit = visiting.flatMap(possibleNeighbours) diff newVisited
        existsAnyPath(newToVisit, newVisited)
      }
    }

    def possibleNeighbours(current: Cell): Set[Cell] = {
      val row = current.x
      val col = current.y
      Set(
      Cell(row - 1, col),
      Cell(row - 1, col + 1),
      Cell(row, col + 1),
      Cell(row, col - 1),
      Cell(row + 1, col),
      Cell(row + 1, col - 1)
      ) filter colorBoard
    }

    existsAnyPath(firstRow, Set())
  }
}