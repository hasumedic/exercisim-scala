case class Cipher(code: Option[String]) {

  private def invalidInput(input: String): Boolean = {
    input.isEmpty || !input.matches( """^[a-z]+$""")
  }

  val key: String = code match {
    case Some(x) => if (invalidInput(x)) throw new IllegalArgumentException else x
    case None => "aaaaaaaaaaaaaaaaaaa"
  }

  def encode(text: String): String = {
    (text.toList zip key.toList).map { case (c1, c2) =>
      val candidate = c1 + c2 - 'a'
      if (candidate > 'z') (((c1 + c2) % 'z') - 1).toChar
      else candidate.toChar
    }.mkString
  }

  def decode(encoded: String): String = {
    (encoded.toList zip key.toList).map { case (c1, c2) => (c1 - c2 + 'a').toChar }.mkString
  }
}
