package euler
package til60

object Euler53 extends EulerProblem {
  override def result() = {
    val combs = for {
      n <- (1 to 100).view
      r <- 1 to n
    } yield combinations(n, r)
    combs.count(_ > 1000000)
  }
}
