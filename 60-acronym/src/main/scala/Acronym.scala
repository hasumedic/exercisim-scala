case class Acronym(phrase: String) {
  def abbreviate: String = {
    words.map(processWord).mkString.toUpperCase
  }

  private def words: Array[String] = {
    phrase.split(" ").flatMap(_.split("-"))
  }

  private def processWord: (String) => String = {
    word =>
      val cleanWord = word.filter(_.isLetter)

      if (isCapitalized(cleanWord)) word.head.toString
      else cleanWord.head + cleanWord.tail.filter(_.isUpper)
  }

  private def isCapitalized(cleanWord: String): Boolean = {
    cleanWord.forall(_.isUpper)
  }
}
