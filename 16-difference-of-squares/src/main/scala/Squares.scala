case class Squares() {
  def squareOfSums(number: Int): Int = square(sumOf(number))

  def sumOfSquares(number: Int): Int = squaresOf(number).sum

  def difference(number: Int): Int = squareOfSums(number) - sumOfSquares(number)

  private def square(number: Int) = number * number

  private def sumOf(number: Int): Int = decreasingRangeOf(number).sum

  private def decreasingRangeOf(number: Int): Range = number to 0 by -1

  private def squaresOf(number: Int): List[Int] = decreasingRangeOf(number).toList.map(square)
}
