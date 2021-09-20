package euler
package til20

object Euler14 extends EulerProblem {
  def seriesLen(n: Long, len: Long = 1): Long = {
    if (n == 1) len
    else if (n % 2 == 0) seriesLen(n / 2, len + 1)
    else seriesLen(3 * n + 1, len + 1)
  }

  override def result = {
    (1L until 1000000L)
      .map(seriesLen(_))
      .zipWithIndex
      .reduceLeft((max, next) => if (next._1 > max._1) next else max)
      ._2 + 1
  }
}
