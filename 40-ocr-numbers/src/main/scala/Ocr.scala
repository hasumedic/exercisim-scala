import Ocr.Row

case class Ocr(string: String) {

  def convert: String = {
    def extractRows: Array[Row] = {
      string
        .split( """\n""")
        .sliding(4, 4)
        .toArray
    }

    def extractPotentialDigits(row: Row): List[String] = {
      row
        .map(row => row.sliding(3, 3))
        .flatMap(_.zipWithIndex)
        .groupBy(_._2)
        .map({ case (k, v) => (k, v.map(_._1)) })
        .toList
        .sortBy(_._1)
        .map(_._2.mkString("\n"))
    }

    def extractDigitsFromRow(row: Row): String = {
      val value = for {
        potentialDigit <- extractPotentialDigits(row)
        digit <- Ocr.representation(potentialDigit)
      } yield digit
      value.mkString
    }

    (for {
      row <- extractRows
    } yield extractDigitsFromRow(row)).mkString(",")
  }
}

object Ocr extends Enumeration {

  type Row = Array[String]

  def representation(string: String): Option[String] = string match {
    case Zero => Some("0")
    case One => Some("1")
    case Two => Some("2")
    case Three => Some("3")
    case Four => Some("4")
    case Five => Some("5")
    case Six => Some("6")
    case Seven => Some("7")
    case Eight => Some("8")
    case Nine => Some("9")
    case _ => Some("?")
  }

  type Number = Value

  val Zero = List(" _ "
    , "| |"
    , "|_|"
    , "   ").mkString("\n")
  val One = List("   "
    , "  |"
    , "  |"
    , "   ").mkString("\n")
  val Two = List(" _ "
    , " _|"
    , "|_ "
    , "   ").mkString("\n")
  val Three = List(" _ "
    , " _|"
    , " _|"
    , "   ").mkString("\n")
  val Four = List("   "
    , "|_|"
    , "  |"
    , "   ").mkString("\n")
  val Five = List(" _ "
    , "|_ "
    , " _|"
    , "   ").mkString("\n")
  val Six = List(" _ "
    , "|_ "
    , "|_|"
    , "   ").mkString("\n")
  val Seven = List(" _ "
    , "  |"
    , "  |"
    , "   ").mkString("\n")
  val Eight = List(" _ "
    , "|_|"
    , "|_|"
    , "   ").mkString("\n")
  val Nine = List(" _ "
    , "|_|"
    , " _|"
    , "   ").mkString("\n")
}
