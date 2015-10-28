package bob

class Bob {
  def hey(message: String): String = {
    if (isSilence(message)) "Fine. Be that way!"
    else if (isShout(message)) "Whoa, chill out!"
    else if (isQuestion(message)) "Sure."
    else "Whatever."
  }

  def isQuestion(message: String): Boolean = {
    message.endsWith("?")
  }

  def isShout(message: String): Boolean = {
    message.toUpperCase == message && message.filter(_.isLetter).length > 0
  }

  def isSilence(words: String) = words.trim.isEmpty
}
