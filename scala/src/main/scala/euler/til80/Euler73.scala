package euler
package til80

object Euler73 extends EulerProblem {

  /**
    * From wikipedia:
    * All pairs of positive coprime numbers (m, n) (with m > n) can be arranged in two disjoint
    * complete ternary trees, one tree starting from (2,1) (for even-odd and odd-even pairs),
    * and the other tree starting from (3,1) (for odd-odd pairs).
    *
    * The children of each vertex (m,n) are generated as follows:
    *  Branch 1: (2m-n,m)
    *  Branch 2: (2m+n,m)
    *  Branch 3: (m+2n,n)
    * This scheme is exhaustive and non-redundant with no invalid members.
    */
  override def result = {
    @annotation.tailrec
    def countFracts(nodes: List[(Int, Int)], cnt: Int): Int = nodes match {
      case Nil                      => cnt
      case (d, _) :: t if d > 12000 => countFracts(t, cnt)
      case (d, n) :: t =>
        val x = n.toDouble / d
        val newCnt = if (x > 1.0 / 3 && x < 0.5) cnt + 1 else cnt
        countFracts((2 * d - n, d) :: (2 * d + n, d) :: (d + 2 * n, n) :: t,
                    newCnt)
    }

    countFracts(List((2, 1), (3, 1)), 0)
  }

}
