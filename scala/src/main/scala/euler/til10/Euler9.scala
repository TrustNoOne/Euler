package euler
package til10

import scala.math.sqrt

object Euler9 extends EulerProblem {

  override def result = {
    val products = for {
      a <- 2 to 999
      b <- a + 1 to 999
      c = sqrt(a.toDouble * a + b * b)
      if a + b + c == 1000
    } yield (a * b * c).toInt

    products.head
  }
}
