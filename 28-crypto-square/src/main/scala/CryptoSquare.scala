case class CryptoSquare() {
  def normalizePlaintext(input: String): String = {
    input.
      filter(_.isLetterOrDigit)
      .toLowerCase
  }

  def squareSize(input: String): Int = {

    def squareRoot(guess: Double, input: Double): Double = {
      def nextGuess(guess: Double, input: Double): Double = (guess + input / guess) / 2

      def isGoodEnough(guess: Double, goal: Double): Boolean = {
        (Math.abs((guess * guess) - goal) / goal) < 0.001
      }

      if (input == 0) 0
      else if (isGoodEnough(guess, input)) guess
      else squareRoot(nextGuess(guess, input), input)
    }

    def normalize(result: Double): Int = {
      if ((result - result.toInt) < 0.001) result.toInt
      else result.toInt + 1
    }

    val realSquareRoot = squareRoot(1, input.length)
    normalize(realSquareRoot)
  }

  def plaintextSegments(input: String): List[String] = {
    val normalized = normalizePlaintext(input)
    val size = squareSize(normalized)
    size match {
      case 0 => List()
      case _ => normalized.grouped(size).toList
    }
  }

  def ciphertext(input: String): String = {
    def printIndexedSegments(indexedSegments: Map[Int, List[(Char, Int)]]): String = {
      (for {
        index <- 0 until indexedSegments.size
        (char, index) <- indexedSegments(index)
      } yield char).mkString
    }

    val segments = plaintextSegments(input)
    val indexedSegments = indexSegments(segments)
    printIndexedSegments(indexedSegments)
  }

  def normalizedCiphertext(input: String): String = {
    def printSpacedIndexedSegments(indexedSegments: Map[Int, List[(Char, Int)]]): String = {
      val groups = for {
        index <- 0 until indexedSegments.size
      } yield indexedSegments(index)
      groups.
        map(list => list.foldLeft("")((acc, tuple) => acc + tuple._1))
        .mkString(" ")
    }

    val segments = plaintextSegments(input)
    val indexedSegments = indexSegments(segments)
    printSpacedIndexedSegments(indexedSegments)
  }

  private def indexSegments(segments: List[String]): Map[Int, List[(Char, Int)]] = {
    (for {
      segment <- segments
      index <- 0 until segment.length
    } yield (segment(index), index)).groupBy(_._2)
  }
}
