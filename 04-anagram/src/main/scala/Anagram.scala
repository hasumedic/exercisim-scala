class Anagram(val word: String) {

  def matches(candidates: Seq[String]): Seq[String] = {

    def wordToMap(wordToMap: String): Map[Char, Int] =
      wordToMap.groupBy(char => char.toLower).mapValues(_.length)

    def sameWords(word: String, anotherWord: String): Boolean =
      word.toLowerCase == anotherWord.toLowerCase

    val wordMap = wordToMap(word)
    candidates.filter(candidate => !sameWords(candidate, word) && wordToMap(candidate) == wordMap)
  }
}
