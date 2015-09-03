class DNA(val code: String) {

  private val baseMap = Map('A' -> 0, 'T' -> 0, 'G' -> 0, 'C' -> 0)

  require(code.forall(char => isDNAChar(char)))

  lazy val codeMap = code.groupBy(c => c).mapValues(_.length)

  def count(char: Char): Int = {
    require(isDNAChar(char))
    codeMap.getOrElse(char, 0)
  }

  def nucleotideCounts(): Map[Char, Int] =
    mergeMaps(baseMap, codeMap)

  private def mergeMaps(map: Map[Char, Int], anotherMap: Map[Char, Int]): Map[Char, Int] =
    map ++ anotherMap.map{ case (k,v) => k -> (v + map.getOrElse(k,0)) }

  private def isDNAChar(char: Char): Boolean = {
    baseMap.contains(char)
  }
}
