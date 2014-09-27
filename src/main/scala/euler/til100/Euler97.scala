package euler
package til100

object Euler97 extends EulerProblem {

  override def result = {
    def digit(n: Int): Iterator[Long] = (1 to n).foldLeft(Iterator.iterate(1L)(_ * 2 % 10))(
      (d, _) => d.scanLeft(0L)((x, d) => x * 2 % 10 + d * 2 / 10))

    val digitIterators = (0 to 9) map digit

    val _2to7830457 = fromDigits((1 to 7830457).view.map(_ => digitIterators.map(_.next())).last)
    (_2to7830457 * 28433 + 1) % 10000000000L
  }

}


