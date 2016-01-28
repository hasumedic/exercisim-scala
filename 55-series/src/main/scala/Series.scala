object Series {
  def slices(pieces: Int, input: String): List[List[Int]] = {
    if (input.isEmpty) List()
    else input.sliding(pieces).map(_.map(_.asDigit).toList).toList
  }
}
