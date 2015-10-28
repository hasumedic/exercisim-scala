case class Garden(names: List[String], plantCode: String) {

  import Plant.Plant

  def getPlants(name: String): List[Plant] = garden.getOrElse(name, List())

  private lazy val garden = plantGarden

  private def plantGarden: Map[String, List[Plant]] = {
    val kids = names.sorted
    val plantCodes = groupPlantCodes()
    plant(kids, plantCodes)
  }

  private def groupPlantCodes(): List[String] = {
    val rows = plantCode.split( """\n""");
    val zipped = rows(0).grouped(2) zip rows(1).grouped(2)
    (for (tuple <- zipped) yield tuple._1 + tuple._2).toList
  }

  private def plant(kids: List[String], plants: List[String]): Map[String, List[Plant]] = {
    (for {
    (kid, plantCodes) <- kids zip plants
    } yield (kid, codesToPlant(plantCodes))).toMap
  }

  private def codesToPlant(plantCodes: String): List[Plant] = {
    (for (plantCode <- plantCodes) yield Plant.fromChar(plantCode)).toList
  }
}

object Garden {
  private val kids = List(
    "Alice", "Bob", "Charlie", "David",
    "Eve", "Fred", "Ginny", "Harriet",
    "Ileana", "Joseph", "Kincaid", "Larry"
  )

  def defaultGarden(plantCode: String): Garden = {
    Garden(kids, plantCode)
  }
}

object Plant {

  sealed trait Plant

  case object Radishes extends Plant

  case object Clover extends Plant

  case object Grass extends Plant

  case object Violets extends Plant

  def fromChar(char: Char): Plant = char match {
    case 'R' => Radishes
    case 'C' => Clover
    case 'G' => Grass
    case 'V' => Violets
  }
}