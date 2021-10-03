package euler
package til50

object Euler45 extends EulerProblem {
  override def result() = {
    val hexagonals = LongIterator.from(0L) map { n => n * (2 * n - 1) }
    hexagonals
      .filter(n => isTriangular(n) && isPentagonal(n))
      .take(3)
      .toList
      .last
  }
}
