package euler
package til110

import euler.Utils.withResource

object Euler102 extends EulerProblem {

  def triangs = withResource("p102_triangles.txt") { f => f.getLines().toList.map(_.split(",").map(_.toLong).toList) }

  override def result() = triangs.count { t =>
    val baryCoords = toBarycentric(t(0), t(1), t(2), t(3), t(4), t(5))(0, 0)
    baryCoords.forall(_ >= 0)
  }

  def toBarycentric(x1: Long, y1: Long, x2: Long, y2: Long, x3: Long, y3: Long)(x: Long, y: Long) = {
    val detT = ((y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)).toDouble
    val λ1 = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / detT
    val λ2 = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / detT
    val λ3 = 1 - λ1 - λ2
    Seq(λ1, λ2, λ3)
  }

}
