package euler
package til90

object Euler86 extends EulerProblem {
  import math._
  // cuboid with edges a <= b <= c
  // calculating the derivative of (sqrt(x^2+b^2) + sqrt((a-x)^2+c^2))
  // we know that the minimum path has length sqrt(c^2+(a+b)^2)
  override def result() = {
    val lens = for {
      c <- Iterator.from(1)
      b <- Range.inclusive(1, c)
      a <- Range.inclusive(1, b)
      sq = sqrt(c.toDouble * c + (a + b) * (a + b))
      if rint(sq) == sq // filter only perfect squares
    } yield c // c is the longest edge in the solution

    lens.drop(1000000).next()
  }

}
