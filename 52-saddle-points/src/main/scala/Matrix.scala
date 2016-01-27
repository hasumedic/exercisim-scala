case class Matrix(matrix: List[List[Int]]) {

  def saddlePoints: Set[(Int, Int)] = {

    def isSaddlePoint(row: Int, col: Int): Boolean = {

      def isBiggestInRow(candidate: Int, row: Int) = {
        matrix(row).max <= candidate
      }

      def isSmallestInColumn(candidate: Int, col: Int): Boolean = {
        matrix.map(_ (col)).min >= candidate
      }

      val candidate = matrix(row)(col)
      isBiggestInRow(candidate, row) && isSmallestInColumn(candidate, col)
    }

    if (matrix.isEmpty) Set()
    else {
      (for {
        row <- matrix.indices
        col <- matrix.indices
        if isSaddlePoint(row, col)
      } yield (row, col)).toSet
    }
  }
}
