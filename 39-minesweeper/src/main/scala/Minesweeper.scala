object Minesweeper {

  private val mine = '*'
  private val empty = ' '

  def annotate(board: List[String]): List[String] = {
    if (board.isEmpty) List()
    else {
      val quickAccessBoard = buildQuickAccessBoard(board)
      val computedBoard = for {
        row <- quickAccessBoard.indices
        col <- quickAccessBoard(row).indices
      } yield computePosition(quickAccessBoard, row, col)
      computedBoardToList(quickAccessBoard(0).length, computedBoard.toArray)
    }
  }

  private def buildQuickAccessBoard(board: List[String]): Array[Array[Char]] = {
    board.toArray.map(_.toCharArray)
  }

  private def computePosition(quickAccessBoard: Array[Array[Char]], row: Int, col: Int): Char = {
    quickAccessBoard(row)(col) match {
      case Minesweeper.mine => Minesweeper.mine
      case Minesweeper.empty => computeNeighbourMines(quickAccessBoard, row, col)
    }
  }

  private def computeNeighbourMines(quickAccessBoard: Array[Array[Char]], row: Int, col: Int): Char = {
    val neighbourMines = (for {
      i <- row - 1 to row + 1
      if i >= 0 && i < quickAccessBoard.length
      j <- col - 1 to col + 1
      if j >= 0 && j < quickAccessBoard(i).length
    } yield if (quickAccessBoard(i)(j) == mine) 1 else 0).sum

    if (neighbourMines == 0) Minesweeper.empty
    else neighbourMines.toString.head
  }

  private def computedBoardToList(length: Int, computedBoard: Array[Char]): List[String] = {
    computedBoard
      .sliding(length, length)
      .map(_.mkString)
      .toList
  }
}

