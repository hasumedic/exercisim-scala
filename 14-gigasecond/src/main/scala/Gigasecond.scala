import java.util.{Calendar, GregorianCalendar}

case class Gigasecond(calendar: GregorianCalendar) {
  def date: GregorianCalendar = {
    calendar.add(Calendar.SECOND, 1000000000)
    calendar
  }
}
