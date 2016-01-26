object PigLatin {
  type Word = String

  private val suffix = "ay"
  private val startsWithVowel = """^[aeiou].*$"""
  private val startsWithQu = """^qu.*"""

  def translateToPigLatin(word: Word): Word = {
    if (word.matches(startsWithVowel)) word + suffix
    else {
      val consonantToAppend = if (word.matches(startsWithQu)) word.take(2) else word.take(1)
      translateToPigLatin(word.substring(consonantToAppend.length, word.length) + consonantToAppend)
    }
  }

  def translate(source: Word): Word = {
    source.split(" ").map(translateToPigLatin).mkString(" ")
  }
}
