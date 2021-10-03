package euler
package til110

import scala.collection.immutable.BitSet

/**
 */
object Euler106 extends EulerProblem {

  /**
   * Only pairs of subset of the same size have to be considered
   * if each element of the firsts sorted subset is greater than the
   * corresponding element in the second sorted subset it's not to be taken into account
   *
   * e.g. for 4 items:
   * ab cd
   * ac bd
   * ad bc -> just this one a < b but d > c
   */
  override def result() = {
    val n = 12
    val set = BitSet(1 to n: _*)
    (2 to n / 2).map(n => solve(set, n)).sum
  }

  def solve(set: BitSet, n: Int) = {
    def loop(remaining: BitSet, result: Iterator[(BitSet, BitSet)]): Iterator[(BitSet, BitSet)] = {
      if (remaining.isEmpty) result
      else {
        val nextRemaining = remaining.tail
        val nextResult = for {
          s1 <- nextRemaining.subsets(n - 1)
          s2 <- (nextRemaining ^ s1).subsets(n)
        } yield (s1 + remaining.head, s2)

        loop(nextRemaining, result ++ nextResult)
      }
    }

    loop(set, Iterator.empty).count { case (s1, s2) =>
      s1 zip s2 exists { case (x, y) => x > y }
    }
  }

}
