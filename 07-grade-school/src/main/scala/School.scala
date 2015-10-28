class School {

  private var classes: List[(Int, String)] = List()

  def db: Map[Int, Seq[String]] =
    classes.groupBy(_._1).mapValues(_.map(_._2))

  def add(student: String, grade: Int): Unit =
    classes = classes :+(grade, student)

  def grade(target: Int): Seq[String] = db.getOrElse(target, Seq())

  def sorted: Map[Int, Seq[String]] = sortMapValues(sortMapKeys(db))

  private def sortMapKeys(map: Map[Int, Seq[String]]): Map[Int, Seq[String]] =
    map.toSeq.sortBy(_._1).toMap

  private def sortMapValues(map: Map[Int, Seq[String]]): Map[Int, Seq[String]] =
    map.toSeq.map(pair => (pair._1, pair._2.sorted)).toMap
}
