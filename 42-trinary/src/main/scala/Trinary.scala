case class TrinaryToIntConversion(value: Int, power: Int)

object Trinary {

  private def isValidTrinary(string: String): Boolean = {
    !string.isEmpty && string.matches( """[0-2]*""")
  }

  def trinaryToInt(string: String): Int = {
    require(isValidTrinary(string))
    string.reverse.foldLeft(TrinaryToIntConversion(0, 0))(
      (conversion, char) => TrinaryToIntConversion(
        conversion.value + char.asDigit * math.pow(3, conversion.power).toInt, conversion.power + 1
      )
    ).value
  }

  def intToTrinary(int: Int): String = {
    def convertToTrinary(i: Int, acc: String): String = {
      if (i < 3) (acc + i).reverse
      else convertToTrinary(i / 3, acc + i % 3)
    }

    convertToTrinary(int, "")
  }
}
