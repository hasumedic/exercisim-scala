case class Matrix(matrix: String) {

  private val matrixArray: Array[Array[String]] =
    matrix
      .split("\n")
      .map(_.split(" "))

  def rows(index: Int): Vector[Int] = matrixArray(index)
    .map(_.toInt)
    .toVector

  def cols(index: Int): Vector[Int] = matrixArray
    .flatMap(_.zipWithIndex)
    .groupBy(_._2)(index)
    .map(_._1.toInt)
    .toVector

  override def equals(other: Any): Boolean = other match {
    case other: Matrix => matrix == other.matrix
    case _ => false
  }
}
