package euler
package til70

import euler._

object Euler61 extends EulerProblem {

  override def result = {
    val polys = (1000 to 9999) filter isPoly

    val cyclics = for {
      p1 <- polys
      p2 <- polys if p1 % 100 == p2 / 100
      p3 <- polys if p2 % 100 == p3 / 100
      p4 <- polys if p3 % 100 == p4 / 100
      p5 <- polys if p4 % 100 == p5 / 100
      p6 <- polys if p5 % 100 == p6 / 100 && p6 % 100 == p1 / 100
    } yield Seq(p1, p2, p3, p4, p5, p6)

    val res = cyclics find { xs =>
      // each number could be of more that one polygonal kind
      val ss = xs map sides
      val sidesCombinations = for {
        s1 <- ss(0)
        s2 <- ss(1)
        s3 <- ss(2)
        s4 <- ss(3)
        s5 <- ss(4)
        s6 <- ss(5)
      } yield Seq(s1, s2, s3, s4, s5, s6)

      sidesCombinations.exists(_.toSet.size == 6)
    }

    res.get.sum
  }

  def isPoly(n: Int) = {
    (3 to 8).find(s => isNgonal(s, n)).isDefined
  }

  def sides(n: Int) = {
    (3 to 8) collect { case s: Int if isNgonal(s, n) => s }
  }

}
