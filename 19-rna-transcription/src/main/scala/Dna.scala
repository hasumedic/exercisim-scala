class Dna {
  def toRna(chain: String): String = chain.map(Dna.transcribe)
}

object Dna {
  def apply() = new Dna

  def transcribe(dna: Char): Char = dna match {
    case 'C' => 'G'
    case 'G' => 'C'
    case 'A' => 'U'
    case 'T' => 'A'
  }
}
