package euler
package til10

object Euler1 extends EulerProblem {
  override def result = (1 to 999 filter (x => x % 5 == 0 && x % 3 == 0)).sum
}
