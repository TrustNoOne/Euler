package euler
package til110

/**
 * The Fibonacci sequence is defined by the recurrence relation:
 *
 * Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
 * It turns out that F541, which contains 113 digits, is the first Fibonacci number for which the last
 * nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order).
 * And F2749, which contains 575 digits, is the first Fibonacci number for which
 * the first nine digits are 1-9 pandigital.
 *
 * Given that Fk is the first Fibonacci number for which the first nine digits
 * AND the last nine digits are 1-9 pandigital, find k.
 */
object Euler104 extends EulerProblem {
  override def result() = {
    PandigitalFibIdxIterator.next()
  }

  private object PandigitalFibIdxIterator extends Iterator[Int] {
    override def hasNext = true

    val digits = Set('1', '2', '3', '4', '5', '6', '7', '8', '9')
    val logsqrt5 = math.log10(math.sqrt(5.0))
    val logPhi = math.log10((1.0 + math.sqrt(5.0)) / 2.0)

    var n = 1
    var (f1, f2) = (0, 1)

    @annotation.tailrec
    override def next(): Int = {
      val last10 = (f1 + f2) % 1000000000
      f1 = f2
      f2 = last10
      n += 1

      if (last10.toString.toSet == digits) {
        val t = n * logPhi - logsqrt5
        val first10 = math.pow(10, t - t.toInt + 8).toInt
        if (first10.toString.toSet == digits) n else next()
      } else next()
    }
  }
}
