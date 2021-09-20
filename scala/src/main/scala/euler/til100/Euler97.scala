package euler
package til100

object Euler97 extends EulerProblem {

  override def result = {
    val _2to7830457 =
      (1 to 7830457).foldLeft(1L)((x, _) => x * 2 % 10000000000L)
    (_2to7830457 * 28433 + 1) % 10000000000L
  }

}
