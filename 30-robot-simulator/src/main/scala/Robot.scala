import Bearing._

case class Robot(bearing: Orientation, coordinates: (Int, Int)) {

  def advance(): Robot = Robot(bearing, bearing match {
    case North => (coordinates._1, coordinates._2 + 1)
    case South => (coordinates._1, coordinates._2 - 1)
    case East => (coordinates._1 + 1, coordinates._2)
    case West => (coordinates._1 - 1, coordinates._2)
  })

  def turnRight(): Robot = Robot(bearing match {
    case North => East
    case South => West
    case East => South
    case West => North
  }, coordinates)

  def turnLeft(): Robot = Robot(bearing match {
    case North => West
    case South => East
    case East => North
    case West => South
  }, coordinates)

  def simulate(movesSequence: String): Robot =
    movesSequence.foldLeft(this)((robot, move) => move match {
      case 'R' => robot.turnRight()
      case 'L' => robot.turnLeft()
      case 'A' => robot.advance()
    })
}

object Bearing {

  sealed trait Orientation

  case object North extends Orientation

  case object East extends Orientation

  case object South extends Orientation

  case object West extends Orientation

}