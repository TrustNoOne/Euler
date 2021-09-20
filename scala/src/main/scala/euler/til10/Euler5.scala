package euler
package til10

object Euler5 extends EulerProblem {
  override def result = {
    val divisors = List(20, 19, 18, 17, 16, 15, 14, 13, 12, 11)
    def go(n: Int): Int = {
      if (divisors forall (n % _ == 0)) n
      else go(n + 1)
    }
    go(1)
  }
}
