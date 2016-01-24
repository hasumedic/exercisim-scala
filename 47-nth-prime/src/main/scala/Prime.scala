object Prime {

  //Naive solution, not very efficient

  def nth(nth: Int): Int = prime drop nth - 1 head

  private def prime: Stream[Int] = 2 #:: Stream.from(3).filter(isPrime)

  private def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)
}
