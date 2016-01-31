import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import concurrent.duration.DurationInt

object Frequency {

  def frequency(threads: Int, chars: Seq[String]): Map[Char, Int] = {

    implicit val executionContext =
      ExecutionContext.fromExecutor(Executors.newWorkStealingPool(threads))

    val stringToProcess = filterInput(chars)
    val frequencies = List(Future(groupChars(stringToProcess)))
    val futureFrequencies = Future.fold(frequencies)(Map[Char, Int]())(consolidateFrecuencies)
    Await.result(futureFrequencies, 2 seconds)
  }

  private def groupChars(stringToProcess: String): Map[Char, Int] = {
    stringToProcess.groupBy(identity).mapValues(_.length)
  }

  private def filterInput(chars: Seq[String]): String = {
    chars.mkString.toLowerCase.filter(_.isLetter)
  }

  def consolidateFrecuencies(map1: Map[Char, Int], map2: Map[Char, Int]): Map[Char, Int] = {
    map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }
  }
}
