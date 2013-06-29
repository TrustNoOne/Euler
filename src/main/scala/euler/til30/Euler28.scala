package euler
package til30

object Euler28 extends EulerProblem {
  def corners: Stream[(Int, Int, Int, Int)] = (0, 0, 0, 1) #:: Stream.from(1).zip(corners).map { n =>
    (n._2._4 + 2 * n._1, n._2._4 + 4 * n._1, n._2._4 + 6 * n._1, n._2._4 + 8 * n._1)
  }

  override def result = corners.take(501).foldLeft(0)((sum, c) => sum + c._1 + c._2 + c._3 + c._4)
}