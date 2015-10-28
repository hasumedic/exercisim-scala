case class Atbash() {

  private val WORDS_GROUP_SIZE = 5

  private def stripInvalidChars(input: String) = input.filter(_.isLetterOrDigit)

  def encode(input: String): String = {
    stripInvalidChars(input)
      .map(char => transcription.getOrElse(char.toLower, char))
      .grouped(WORDS_GROUP_SIZE)
      .mkString(" ")
  }

  private lazy val transcription: Map[Char, Char] = (('a' to 'z') zip ('a' to 'z').reverse).toMap
}
