package euler
package til90

object Euler85 extends EulerProblem {

  //3x2    3x3   4x2
  //6 4 2  9 6 3 8 6 4 2
  //3 2 1  6 4 2 4 3 2 1
  //       3 2 1
  //
  //XxY => Y+2Y+...+XY + (Y-1)+2*(Y-1)+...+X*(Y-1) + 1+2+...+X = (X*(X+1)/2)*(Y*(Y+1)/2)

  override def result() = {
    val cr = for {
      w <- 1 to 1000
      h <- w to 1000
    } yield (h, w, containedRects(h, w))

    val res = cr.minBy(x => math.abs(x._3 - 2000000))
    res._1 * res._2
  }

  def containedRects(w: Int, h: Int) = (w * (w + 1) / 2) * (h * (h + 1) / 2)

}
