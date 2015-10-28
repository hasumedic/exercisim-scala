case class Position(x: Int, y: Int)

class Board(queenA: Position, queenB: Position) {

  override def toString(): String = {
    val board = (for {
      row <- 0 to 7
      col <- 0 to 7
    } yield {
        if (Position(row, col) == queenA) 'W'
        else if (Position(row, col) == queenB) 'B'
        else '_'
      }).mkString

    board
      .grouped(8)
      .map(_.grouped(1).mkString(" "))
      .map(_ + "\n")
      .mkString
  }
}

object Board {
  def apply(queenA: Option[Position], queenB: Option[Position]): Board = {
    val positionA = queenA.getOrElse(Position(-1, -1))
    val positionB = queenB.getOrElse(Position(-1, -1))
    new Board(positionA, positionB)
  }
}

case class Queens() {
  def boardString(queen1: Option[Position], queen2: Option[Position]): String =
    Board(queen1, queen2).toString()

  def canAttack(fromPosition: Position, toPosition: Position): Boolean = {
    isAttackableHorizontally(fromPosition, toPosition) ||
      isAttackableVertically(fromPosition, toPosition) ||
      isAttackableDiagonally(fromPosition, toPosition)
  }

  private def isAttackableHorizontally(fromPosition: Position, toPosition: Position): Boolean =
    fromPosition.x == toPosition.x

  private def isAttackableVertically(fromPosition: Position, toPosition: Position): Boolean =
    fromPosition.y == toPosition.y

  private def isAttackableDiagonally(fromPosition: Position, toPosition: Position): Boolean =
    horizontalDistance(fromPosition, toPosition) == verticalDistance(fromPosition, toPosition)

  private def horizontalDistance(fromPosition: Position, toPosition: Position): Int =
    Math.abs(fromPosition.x - toPosition.x)

  private def verticalDistance(fromPosition: Position, toPosition: Position): Int =
    Math.abs(fromPosition.y - toPosition.y)
}
