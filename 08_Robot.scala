import scala.util.Random

class Robot {

  private var _name = Robot.newName()

  def name: String = _name

  def reset() = _name = Robot.newName()
}

object Robot {
  def newName(): String = {
    randomString(2) + randomNumber(3)
  }

  private def randomString(length: Int): String =
    Random.shuffle(('a' to 'z').toList).take(length).mkString

  private def randomNumber(length: Int): String =
    Random.shuffle((1 to 9).toList).take(length).mkString
}
