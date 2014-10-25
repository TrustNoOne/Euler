package euler
package til10

object Euler9 extends EulerProblem {
  import scala.math.sqrt
  override def result = {
    for (a <- 2 to 999; b <- a + 1 to 999 if a + b < 1000 if a + b + sqrt(a * a + b * b) == 1000)
      yield (a, b, sqrt(a * a + b * b))
  }
}