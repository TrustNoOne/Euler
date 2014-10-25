package euler
package til80

import scala.annotation.tailrec

object Euler75 extends EulerProblem {

  /* see Euler39
   * 
   * a = m^2-n^2 b = 2mn c = m^2+n^2
   * a+b+c = 2*m^2 +2mn = 2m(m+n)
   * 2m(m+n) <= 1000 
   */
  override def result = {
    import math._
    val maxL = 1500000

    val primitiveTriples = for {
      n <- 1 until maxL
      m <- n + 1 to ((sqrt(n * n + 2 * maxL) - n) / 2).toInt by 2
      if gcd(m, n) == 1
    } yield (m * m - n * n, 2 * m * n, m * m + n * n)

    val allTriples = primitiveTriples flatMap { t =>
      def loop(i: Int, l: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
        if (i * (t._1 + t._2 + t._3) > maxL) l
        else loop(i + 1, (i * t._1, i * t._2, i * t._3) :: l)
      }
      loop(1, Nil)
    }

    val res = allTriples.groupBy(x => x._1 + x._2 + x._3) filter { case (_, triangs) => triangs.size == 1 }
    res.size
  }

  // slower 
  //    val allTriples = for {
  //      n <- 1 until maxL
  //      m <- n + 1 to ((sqrt(n * n + 2 * maxL) - n) / 2).toInt by 2
  //      if gcd(m, n) == 1
  //      a = m * m - n * n
  //      b = 2 * m * n
  //      c = m * m + n * n
  //      i <- Iterator.from(1).takeWhile(_ * (a + b + c) <= maxL)
  //    } yield (i * a, i * b, i * c)

}

