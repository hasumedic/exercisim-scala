import java.util.GregorianCalendar
import java.util.Calendar
import java.util.Locale

class Meetup(month: Int, year: Int) {

  lazy val gregorianMonth = month - 1

  def teenth(soughtAfterDay: String): GregorianCalendar = {
    val teenthDays = 12 to 19
    lastInRange(soughtAfterDay, teenthDays)
  }

  def first(soughtAfterDay: String): GregorianCalendar = {
    val firstWeek = 1 to 7
    firstInRange(soughtAfterDay, firstWeek)
  }

  def second(soughtAfterDay: String): GregorianCalendar = {
    val secondWeek = 8 to 14
    firstInRange(soughtAfterDay, secondWeek)
  }

  def third(soughtAfterDay: String): GregorianCalendar = {
    val thirdWeek = 15 to 21
    firstInRange(soughtAfterDay, thirdWeek)
  }

  def fourth(soughtAfterDay: String): GregorianCalendar = {
    val fourthWeek = 22 to 28
    firstInRange(soughtAfterDay, fourthWeek)
  }

  def last(soughtAfterDay: String): GregorianCalendar = {
    val firstOfTheMonthDate = new GregorianCalendar(year, gregorianMonth, 1)
    val reverseMonthRange = firstOfTheMonthDate.getActualMaximum(Calendar.DAY_OF_MONTH) to 21 by -1
    firstInRange(soughtAfterDay, reverseMonthRange)
  }

  private def firstInRange(soughtAfterDay: String, range: Range): GregorianCalendar =
    lookupInRange(soughtAfterDay, range).head

  private def lastInRange(soughtAfterDay: String, range: Range): GregorianCalendar =
    lookupInRange(soughtAfterDay, range).last

  private def lookupInRange(soughtAfterDay: String, range: Range): Seq[GregorianCalendar] = {
    for {
      dayNumber <- range
      date = new GregorianCalendar(year, gregorianMonth, dayNumber)
      if isSoughtAfterDay(soughtAfterDay, date)
    } yield date
  }

  private def isSoughtAfterDay(soughtAfterDay: String, date: GregorianCalendar): Boolean =
    date.getDisplayName(Calendar.DAY_OF_WEEK, Calendar.LONG, Locale.getDefault) == soughtAfterDay
}

object Meetup {

  def Mon = "Monday"

  def Tue = "Tuesday"

  def Wed = "Wednesday"

  def Thu = "Thursday"

  def Fri = "Friday"

  def Sat = "Saturday"

  def Sun = "Sunday"

  def apply(month: Int, year: Int) = new Meetup(month, year)
}
