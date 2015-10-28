case class RomanNumeral(decimal: Int) {

  private def convert(number: Int, conversionList: List[(Int, String)], roman: String): String = conversionList match {
    case List() => roman
    case (unit, symbol) :: restConvesionList => {
      if (number - unit >= 0) convert(number - unit, conversionList, roman + symbol)
      else convert(number, restConvesionList, roman)
    }
  }

  def value: String = {
    convert(decimal, RomanNumeral.conversionTable, "")
  }
}

object RomanNumeral {
  def conversionTable: List[(Int, String)] = List(
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (3, "III"),
    (2, "II"),
    (1, "I")
  )
}
