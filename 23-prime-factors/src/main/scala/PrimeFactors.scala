case class PrimeFactors() {
  def primeFactors(factor: Long): List[Long] = {
    if (factor < 2) List()
    else {
      val nextFactor = calculateFirstPrimeFactor(factor)
      List(nextFactor) ++ primeFactors(factor / nextFactor)
    }
  }

  private def calculateFirstPrimeFactor(factor: Long): Long = {
    require(factor >= 2)

    (for {
      n <- (2L to factor).toList
      if factor % n == 0
    } yield n).head
  }
}
