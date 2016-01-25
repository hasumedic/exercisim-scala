import scala.collection.immutable.IndexedSeq

case class PalindromeProducts(from: Int, to: Int) {

  type Palindromes = IndexedSeq[(Int, Set[(Int, Int)])]

  lazy val allPalindromes: Palindromes = groupAndSortPalindromes(calculateAllPalindromes)


  def smallest: (Int, Set[(Int, Int)]) = {
    allPalindromes.head
  }

  def largest: (Int, Set[(Int, Int)]) = {
    allPalindromes.last
  }

  private def calculateAllPalindromes: Palindromes = {
    for {
      x <- from to to
      y <- from to to
      if isPalindrome(x * y)
    } yield (x * y, Set((math.min(x, y), math.max(x, y))))
  }

  private def groupAndSortPalindromes(palindromes: Palindromes): Palindromes = {
    palindromes
      .groupBy(_._1)
      .map { case (k, v) => (k, v.foldLeft(Set[(Int, Int)]())((acc, tuple) => acc ++ tuple._2)) }
      .toIndexedSeq
      .sortBy(_._1)
  }

  private def isPalindrome(number: Int): Boolean = number.toString == number.toString.reverse
}
