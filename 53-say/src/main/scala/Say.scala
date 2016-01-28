object Say {

  val mapping = Map(
    0L -> "zero",
    1L -> "one",
    2L -> "two",
    3L -> "three",
    4L -> "four",
    5L -> "five",
    6L -> "six",
    7L -> "seven",
    8L -> "eight",
    9L -> "nine",
    10L -> "ten",
    11L -> "eleven",
    12L -> "twelve",
    13L -> "thirteen",
    14L -> "fourteen",
    15L -> "fifteen",
    16L -> "sixteen",
    17L -> "seventeen",
    18L -> "eighteen",
    19L -> "nineeen",
    20L -> "twenty",
    30L -> "thirty",
    40L -> "forty",
    50L -> "fifty",
    60L -> "sixty",
    70L -> "seventy",
    80L -> "eighty",
    90L -> "ninety"
  )

  val units = List(
    "",
    " thousand",
    " million",
    " billion",
    " trillion"
  )


  def inEnglish(number: Long): Option[String] = {
    if (isOutOfBounds(number)) None
    else if (number == 0) Some("zero")
    else translateNumber(number)
  }

  def isOutOfBounds(number: Long): Boolean = {
    number < 0 || number >= 1000000000000L
  }

  def truncate(number: Long): List[Long] = {
    number.toString.reverse.sliding(3, 3).map(_.reverse.toLong).toList
  }

  def translateNumber(number: Long): Some[String] = {
    Some(
      truncate(number)
        .zip(units)
        .flatMap { case (triplet, unit) => sayNumber(triplet % 1000).map(_ + unit) }
        .reverse
        .mkString(" ")
    )
  }

  def sayNumber(number: Long): Option[String] = {
    if (number == 0) None
    else if (number < 20) buildSpecialDigits(number)
    else if (number < 100) buildTwoDigits(number)
    else buildThreeDigits(number)
  }

  def buildSpecialDigits(number: Long): Option[String] = {
    Some(mapping(number))
  }

  def buildTwoDigits(number: Long): Option[String] = {
    if (number == 0) None
    else if (number % 10 == 0) buildSpecialDigits(number.toInt)
    else Some(mapping(number / 10 * 10) + "-" + buildSpecialDigits(number % 10).getOrElse(""))
  }

  def buildThreeDigits(number: Long): Option[String] = {
    Some(mapping(number / 100) + " " + "hundred" + buildTwoDigits(number % 100).map(" " + _).getOrElse(""))
  }
}
