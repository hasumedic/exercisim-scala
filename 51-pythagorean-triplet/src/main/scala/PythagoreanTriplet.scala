object PythagoreanTriplet {
  type Triplet = (Int, Int, Int)


  def isPythagorean(triplet: Triplet): Boolean = {
    def square(value: Int): Int = value * value

    triplet._1 != triplet._2 &&
      triplet._2 != triplet._3 &&
      (square(triplet._1) + square(triplet._2) == square(triplet._3) ||
        square(triplet._2) + square(triplet._3) == square(triplet._1) ||
        square(triplet._3) + square(triplet._1) == square(triplet._2))
  }

  def pythagoreanTriplets(from: Int, to: Int): Seq[Triplet] = {
    (for {
      a <- from to to
      b <- from to to
      c <- from to to
      if isPythagorean((a, b, c))
    } yield {
      val list = List(a, b, c).sorted
      list match {
        case x :: y :: z :: rest => (x, y, z)
      }
    }).distinct
  }
}
