class Phrase(val input: String) {

  lazy val words = toWords(filterInput())

  def wordCount(): Map[String, Int] = {
    words.groupBy(w => w.toLowerCase).mapValues(_.length)
  }

  def filterInput(): String = {
    val onlyValidCharacters = stripInvalidCharactersFromInput()
    val separatedBySpaces = transformSeparatorsToSpaces(onlyValidCharacters)
    removeExtraWhiteSpaces(separatedBySpaces)
  }

  def stripInvalidCharactersFromInput() = input.filter(char => isAllowedChar(char))

  def isAllowedChar(char: Char): Boolean = {
    char.isLetterOrDigit || char.isWhitespace || char == ',' || char == '\''
  }

  def transformSeparatorsToSpaces(input: String): String = {
    input.replaceAll(",+", " ")
  }

  def removeExtraWhiteSpaces(input: String): String = {
    input.trim.replaceAll(" +", " ")
  }

  def toWords(sentence: String): Array[String] = {
    sentence.split(" ")
  }
}
