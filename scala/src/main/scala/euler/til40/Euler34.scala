package euler
package til40

object Euler34 extends EulerProblem {
  override def result() = {
    val factorials = 0 to 9 map (n => fact(n).toInt)
    (1 to factorials.sum).filter { n =>
      val digits = toDigits(n)
      digits.size > 1 && n == digits.map(factorials).sum
    }.sum
  }
}
