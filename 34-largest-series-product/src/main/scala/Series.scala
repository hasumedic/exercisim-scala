object Series {

  def digits(digitString: String): List[Int] = {
    digitString.map(_.asDigit).toList
  }

  def slices(size: Int, digitString: String): List[List[Int]] = {

    def sliceAcc(size: Int, digitString: String, acc: List[List[Int]]): List[List[Int]] = {
      if (digitString.length < size) acc
      else sliceAcc(size, digitString.drop(1), acc ++ List(digits(digitString.take(size))))
    }

    sliceAcc(size, digitString, Nil)
  }

  def largestProduct(size: Int, digitString: String): Int = {
    if (digitString.isEmpty || digitString.length < size) 1
    else {
      val products = for {
        slices <- slices(size, digitString)
      } yield slices.product

      products.max
    }
  }
}
