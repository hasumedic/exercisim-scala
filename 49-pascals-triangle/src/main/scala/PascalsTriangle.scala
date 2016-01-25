object PascalsTriangle {
  def triangle(depth: Int): List[List[Int]] = {
    (1 to depth).foldLeft(List[List[Int]]())((acc, depth) =>
      if (acc.isEmpty) acc :+ generateNext(Nil)
      else acc :+ generateNext(acc.last)
    )
  }

  def generateNext(previous: List[Int]): List[Int] = {
    if (previous.isEmpty) List(1)
    else if (previous.size == 1) List(1, 1)
    else List(1) ++ previous.sliding(2).map(_.sum) ++ List(1)
  }
}
