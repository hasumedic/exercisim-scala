object Grains {
  def square(number: Int): BigInt = BigInt(2).pow(number - 1)

  def total: BigInt = square(65) - 1
}
