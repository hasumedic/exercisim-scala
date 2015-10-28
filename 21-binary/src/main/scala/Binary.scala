class Binary(input: String) {
  def toDecimal: Int = {
    if (invalidInput) 0
    else binToDec
  }

  private def binToDec: Int = {
    input.foldRight(0, 0)((char, tuple) => {
      if (char == '1') (tuple._1 + 1, scala.math.pow(2, tuple._1).toInt + tuple._2)
      else (tuple._1 + 1, tuple._2)
    })._2
  }

  private def invalidInput: Boolean = {
    input.isEmpty || input.exists(char => char != '0' && char != '1')
  }
}

object Binary {
  def apply(input: String) = new Binary(input)
}