case class PositionAccumulator(pos: Int, acc: Int)

object Hexadecimal {

  private val Hexadecimal = 16

  private def isValidHex(hex: String) = {
    !hex.isEmpty && hex.forall(c => (0 to 'f').contains(c.toLower))
  }

  def hexToInt(hex: String): Int = {
    if (!isValidHex(hex)) 0
    else {
      hex.foldRight(PositionAccumulator(0, 0))((char, posAcc) =>
        PositionAccumulator(
          posAcc.pos + 1,
          conversionTable(char.toLower) * scala.math.pow(Hexadecimal, posAcc.pos).toInt + posAcc.acc)
      ).acc
    }
  }

  lazy private val conversionTable = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'a' -> 10,
    'b' -> 11,
    'c' -> 12,
    'd' -> 13,
    'e' -> 14,
    'f' -> 15
  )
}
