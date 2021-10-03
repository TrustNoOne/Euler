package euler
package til110

import euler.Utils.withResource

import scala.collection.parallel.CollectionConverters._

/**
 * Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if
 * for any two non-empty disjoint subsets, B and C, the following properties are true:
 * S(B) ≠ S(C); that is, sums of subsets cannot be equal.
 * If B contains more elements than C then S(B) > S(C).
 *
 * For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because 65 + 87 + 88 = 75 + 81 + 84,
 * whereas {157, 150, 164, 119, 79, 159, 161, 139, 158} satisfies both rules
 * for all possible subset pair combinations and S(A) = 1286.
 *
 * Using sets.txt (right click and "Save Link/Target As..."), a 4K text file with one-hundred sets
 * containing seven to twelve elements (the two examples given above are the first two sets in the file),
 * identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).
 */
object Euler105 extends EulerProblem {

  /**
   * p1: S(B) ≠ S(C); that is, sums of subsets cannot be equal.
   * p2: If B contains more elements than C then S(B) > S(C).
   */
  def isSpecialSumSet(xs: Set[Int]) = {
    val checks = for {
      b <- xs.subsets()
      c <- (xs -- b).subsets()
      bSum = b.sum
      cSum = c.sum

      p1 = bSum == 0 || cSum == 0 || bSum != cSum
      p2 = (b.size == c.size) || (b.size > c.size && bSum > cSum) || (cSum > bSum)
    } yield p1 && p2

    checks forall (_ == true)
  }

  override def result() = {
    parseSets().par.filter(isSpecialSumSet).map(_.sum).sum
  }

  def parseSets() = withResource("p105_sets.txt") { src =>
    src
      .getLines()
      .map { line => line.split(",").view.map(_.toInt).toSet }
      .toList
  }

}
