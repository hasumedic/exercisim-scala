import Clock._

class Clock(val _hours: Hours, val _minutes: Minutes) {

  private val hoursOfADay: Hours = 24

  def hours: Int = _hours % hoursOfADay

  def minutes: Int = _minutes % 60

  def +(other: Clock): Clock = new Clock(hours + other.hours, minutes + other.minutes)

  def -(other: Clock): Clock = {
    def changesHour: Boolean = minutes < other.minutes
    def isSameDay(extraHour: Int): Boolean = (hours - (other.hours + extraHour)) > 0
    def subtractHours(extraHour: Int): Hours = hours - (other.hours + extraHour)
    def subtractMinutes: Minutes = Math.abs(minutes - other.minutes % 60)

    val extraHour = if (changesHour) 1 else 0
    val newClockHours =
      if (isSameDay(extraHour)) subtractHours(extraHour)
      else hoursOfADay + subtractHours(extraHour)
    new Clock(newClockHours, subtractMinutes)
  }

  override def equals(other: Any) = other match {
    case other: Clock => hours == other.hours && minutes == other.minutes
    case _ => false
  }

  override def toString: String = f"$hours%02d" + ":" + f"$minutes%02d"
}

object Clock {
  type Hours = Int
  type Minutes = Int

  def apply(hours: Hours, minutes: Minutes) = new Clock(hours, minutes)

  def apply(minutes: Minutes) = {
    new Clock(numberOfHours(minutes), minutes % 60)
  }

  def numberOfHours(minutes: Minutes): Minutes = {
    Math.floor(minutes / 60).toInt
  }
}
