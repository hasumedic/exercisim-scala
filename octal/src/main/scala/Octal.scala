case class ConversionAccumulator(value: Int, acc: Int)

object Octal {

  private def isValidOctal(string: String): Boolean = {
    !string.isEmpty && string.matches( """[0-7]*""")
  }

  def octalToInt(string: String): Int = {
    require(isValidOctal(string))

    def convertNextDigit(converter: ConversionAccumulator, char: Char): ConversionAccumulator = {
      ConversionAccumulator(
        converter.value + (char.asDigit * math.pow(8, converter.acc).toInt), converter.acc + 1)
    }

    string
      .reverse
      .foldLeft(ConversionAccumulator(0, 0))(
        (converter, char) => convertNextDigit(converter, char)
      ).value
  }

  def intToOctal(i: Int): String = {
    def fromIntToOctal(int: Int, acc: String): String =
      if (int < 8) (acc + int).reverse
      else fromIntToOctal(int / 8, acc + (int % 8))

    fromIntToOctal(i, "")
  }
}
