package euler
package til20

object Euler19 extends EulerProblem {
  import java.util._
  import Calendar._
  override def result() = { //procedural...
    val currDate = new GregorianCalendar(1901, JANUARY, 1)
    val endDate = new GregorianCalendar(2001, JANUARY, 1)
    var count = 0
    while (currDate.before(endDate)) {
      if (currDate.get(DAY_OF_WEEK) == 1 && currDate.get(DAY_OF_MONTH) == 1)
        count = count + 1
      currDate.add(DAY_OF_YEAR, 1)
    }
    count
  }
}
