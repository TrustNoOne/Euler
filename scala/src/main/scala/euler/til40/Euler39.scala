package euler
package til40

object Euler39 extends EulerProblem {
  /* a = m^2-n^2 b=2mn c = m^2+n^2
   * a+b+c = 2*m^2 +2mn = 2m(m+n)
   * 2m(m+n) <= 1000 
   */
  override def result = {
    val primitiveTriples = for {
      n <- 1 to 999
      m <- n + 1 to 1000 by 2
      if 2 * m * (m + n) <= 1000
      if (2 to m / 2) forall { x => m % x > 0 || n % x > 0 }
    } yield (m * m - n * n, 2 * m * n, m * m + n * n)

    val allTriples = primitiveTriples flatMap { t =>
      def recursion(i: Int, l: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
        if (i * (t._1 + t._2 + t._3) > 1000) l
        else recursion(i + 1, (i * t._1, i * t._2, i * t._3) :: l)
      }
      recursion(1, Nil)
    }

    allTriples.map(t => t._1 + t._2 + t._3).groupBy(identity).mapValues(_.size).maxBy(_._2)
  }

}