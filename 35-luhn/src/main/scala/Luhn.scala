import scala.collection.immutable.IndexedSeq

case class Luhn(seed: Long) {
  private def computeLuhn(listWithIndexes: IndexedSeq[(Int, Int)]) = {
    def isEven(value: Int): Boolean = {
      value % 2 == 0
    }

    listWithIndexes.map { case (digit: Int, index: Int) =>
      if (isEven(index)) digit
      else {
        val double = digit * 2
        if (double > 9) double - 9
        else double
      }
    }
  }

  def checkDigit: Long = seed % 10

  def addends: List[Int] = {
    val seedIntegerList = seed.toString.map(_.asDigit)
    val reversedIndexedZippedList = seedIntegerList.reverse.zipWithIndex

    val reversedAddens = computeLuhn(reversedIndexedZippedList)

    reversedAddens.reverse.toList
  }

  def checksum: Int = addends.sum % 10

  def isValid: Boolean = checksum == 0

  def create: Long = {
    val candidates = (0 to 9) map (seed * 10 + _)
    candidates.filter(Luhn(_).isValid).head
  }
}
