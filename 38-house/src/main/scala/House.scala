object House {

  private val beginings = List(
    "This is the horse and the hound and the horn",
    "This is the farmer sowing his corn",
    "This is the rooster that crowed in the morn",
    "This is the priest all shaven and shorn",
    "This is the man all tattered and torn",
    "This is the maiden all forlorn",
    "This is the cow with the crumpled horn",
    "This is the dog",
    "This is the cat",
    "This is the rat",
    "This is the malt",
    "This is the house that Jack built."
  )

  private val responses = List(
    "that belonged to the farmer sowing his corn",
    "that kept the rooster that crowed in the morn",
    "that woke the priest all shaven and shorn",
    "that married the man all tattered and torn",
    "that kissed the maiden all forlorn",
    "that milked the cow with the crumpled horn",
    "that tossed the dog",
    "that worried the cat",
    "that killed the rat",
    "that ate the malt",
    "that lay in the house that Jack built."
  )


  def rhyme: String = {
    def prepareRhyme(first: String, rest: List[String]): String = {
      if (rest.isEmpty) first
      else (List(first) ++ rest).mkString("\n")
    }

    def appendRhymeEnding(rhyme: String): String = rhyme + "\n\n"

    def buildRhyme(rhyme: String, beginning: List[String], tail: List[String]): String = {
      val currentRhyme: String = appendRhymeEnding(prepareRhyme(beginning.head, tail))
      val accumulatedRhyme = currentRhyme + rhyme

      if (tail.isEmpty) accumulatedRhyme
      else buildRhyme(accumulatedRhyme, beginning.tail, if (tail.isEmpty) tail else tail.tail)
    }

    buildRhyme("", beginings, responses)
  }
}
