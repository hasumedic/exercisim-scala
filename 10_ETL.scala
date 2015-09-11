object ETL {
  def transform(input: Map[Int, Seq[String]]): Map[String, Int] = {
    for {
      mapElement <- input
      word <- mapElement._2
    } yield (word.toLowerCase, mapElement._1)
  }
}
