package euler
package til100

object Euler91 extends EulerProblem {
  override def result() = {
    val max = 50

    val xs = for {
      x1 <- (0 to max).view
      x2 <- x1 to max if x1 != 0 || x2 != 0
      y1 <- 0 to max if x1 != 0 || y1 != 0
      y2 <- 0 to max if x1 != x2 || y2 > y1
    } yield (x1, y1, x2, y2)

    xs.count { case (x1, y1, x2, y2) => isRight(x1, y1, x2, y2) }
  }

  def isRight(x1: Int, y1: Int, x2: Int, y2: Int) = {
    // three sides (square)
    val a = x1 * x1 + y1 * y1
    val b = x2 * x2 + y2 * y2
    val c = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

    // sort and check for pythagorean theorem
    val xs = List(a, b, c).sorted
    xs(0) + xs(1) == xs(2)
  }

}
