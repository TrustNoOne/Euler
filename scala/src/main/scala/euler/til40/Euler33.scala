package euler
package til40

object Euler33 extends EulerProblem {
  override def result = {
    val curiousFrac = for ( // nx/xd = n/d already 4, no need to check xn/dx etc
      x <- 1 to 9; n <- 0 to 9; d <- 1 to 9 if (n < x || n == x && x < d) &&
        (n * 10 + x) * d == n * (x * 10 + d)
    ) yield ((n * 10 + x), (x * 10 + d))
    val curiousProd = curiousFrac.reduceLeft((prod, x) => (prod._1 * x._1, prod._2 * x._2))
    curiousProd._2 / curiousProd._1 // it's 100, no need to simplify
  }
}
