class Year(val year: Int) {

  private def isDivisibleBy(number: Int): Boolean = year % number == 0

  private def isExceptionalCentury: Boolean = year % 400 == 0

  def isLeap: Boolean = {
    (isDivisibleBy(4) && !isDivisibleBy(100)) || isExceptionalCentury
  }
}

object Year {
  def apply(year: Int): Year = new Year(year)
}
