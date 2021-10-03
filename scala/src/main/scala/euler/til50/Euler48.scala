package euler
package til50

object Euler48 extends EulerProblem {
  override def result() =
    (1 to 1000).map(n => BigInt(n).pow(n)).sum % 10000000000L
}
