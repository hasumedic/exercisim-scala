object Sieve {

  def primesUpTo(limit: Int): List[Int] = {
    sieve(from(2)).takeWhile(_ <= limit).toList
  }

  private def from(n: Int): Stream[Int] = {
    n #:: from(n + 1)
  }

  private def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
  }
}
