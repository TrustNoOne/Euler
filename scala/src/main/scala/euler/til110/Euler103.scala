package euler
package til110

import scala.collection.immutable.BitSet

/**
  * Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if
  * for any two non-empty disjoint subsets, B and C, the following properties are true:
  *
  * S(B) ≠ S(C); that is, sums of subsets cannot be equal.
  * If B contains more elements than C then S(B) > S(C).
  * If S(A) is minimised for a given n, we shall call it an optimum special sum set.
  *
  * The first five optimum special sum sets are given below.
  *
  * n = 1: {1}
  * n = 2: {1, 2}
  * n = 3: {2, 3, 4}
  * n = 4: {3, 5, 6, 7}
  * n = 5: {6, 9, 11, 12, 13}
  *
  * It seems that for a given optimum set, A = {a1, a2, ... , an},
  * the next optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b},
  * where b is the "middle" element on the previous row.
  *
  * By applying this "rule" we would expect the optimum set for n = 6 to be
  * A = {11, 17, 20, 22, 23, 24}, with S(A) = 117.
  * However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set.
  * The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding set string: 111819202225.
  *
  * Given that A is an optimum special sum set for n = 7, find its set string.
  */
object Euler103 extends EulerProblem {
  /*
    6 forb(1: 6)
    ...
    BitSet(12, 14), C: BitSet(6, 9, 10)
    6 9 forb(0: 0, 1: 6 9 /2: 15)
    6 9 10 m14 forb(0: 0, 1: 6 9 10 / 2: 15 16 19 / u3: 25)
    6 9 10 11 forb(0: 0, 1: 6 9 10 11, 2: 15 16 17 19 20 21, 3: 25 26 27 30, 4: 36)
    6 9 10 12 forb(0: 0, 1: 6 9 10 12, 2: 15 16 18 19 21 22, 3: 25 27 28 31, 4: 37)
    6 9 10 12 14  --> 2: 20 23 24 26 (no intersect, no >= min 3)

    6 9 11 12 13
   */

  /**
    * returns None if one of the conditions doesn't hold
    * p1: S(B) ≠ S(C); that is, sums of subsets cannot be equal.
    * p2: If B contains more elements than C then S(B) > S(C).
    */
  def updateForbidden(
      pos: Int,
      newElem: Int,
      forbiddenSums: Map[Int, BitSet]): Option[Map[Int, BitSet]] =
    if (pos == 0) Some(forbiddenSums)
    else {
      //println(s"upd: pos=$pos  newElem=$newElem forb=$forbiddenSums")
      // create the new forbidden sums for this position adding the element to the previous position forbidden sums
      val thisPosForbidden = forbiddenSums.getOrElse(pos, BitSet.empty)
      val newForbidden = forbiddenSums(pos - 1) map (_ + newElem)
      val nextPosMinForbidden = forbiddenSums
        .get(pos + 1)
        .map(_.head) getOrElse Int.MaxValue

      val p1 = (newForbidden & thisPosForbidden).isEmpty
      val p2 = nextPosMinForbidden > newForbidden.max

      if (p1 && p2) {
        val updatedForbiddenSums =
          forbiddenSums.updated(pos, thisPosForbidden | newForbidden)
        updateForbidden(pos - 1, newElem, updatedForbiddenSums)
      } else None
    }

  def solve(size: Int, startElem: Int) = {
    def loop(loopSize: Int,
             current: BitSet,
             elem: Int,
             pos: Int,
             forbiddenSums: Map[Int, BitSet]): Option[BitSet] = {
      //println(s"size=$loopSize elem=$elem pos=$pos current=$current forb=$forbiddenSums")

      if (pos > loopSize) Some(current)
      else {
        // maximum allowed value = sum of first two minus remaining positions from end
        val maxVal = (for {
          first <- current.headOption
          second <- current.tail.headOption
        } yield first + second - loopSize + pos - 1) getOrElse Int.MaxValue - 1

        def findSolution(elem: Int, bestSoFar: Int): Option[BitSet] = {
          // update forbidden sums with the current element and recursively search the next element using the updated sums
          updateForbidden(pos, elem, forbiddenSums) flatMap {
            newForbiddenSums =>
              loop(loopSize,
                   current + elem,
                   elem + 1,
                   pos + 1,
                   newForbiddenSums)
          } match {
            case Some(result)
                if result.sum >= bestSoFar => // found a result but it's worse than the best one
              None

            case result
                if elem == maxVal => // can't go any further, return what we have
              result

            case None => // try the next one and see if we're lucky
              findSolution(elem + 1, bestSoFar)

            case Some(result) => // new best!
              val newBest = result.sum
              val remaining = result.size - pos + 1

              // if in theory a new better solution could be found, keep searching
              val minIncrease = elem * remaining + (remaining * (remaining + 1) / 2)
              if (current.sum + minIncrease < newBest) {
                findSolution(elem + 1, newBest) match {
                  case Some(newResult) if newResult.sum < newBest =>
                    Some(newResult)
                  case _ => Some(result)
                }
              } else {
                Some(result)
              }
          }
        }

        findSolution(elem, bestSoFar = Int.MaxValue)
      }
    }

    loop(size,
         current = BitSet.empty,
         elem = startElem,
         pos = 1,
         Map(0 -> BitSet(0))).get
  }

  override def result = {
    // find next one using the middle element of the previous one as start element
    val resultSize = 7
    val (result, _) =
      Iterator.range(1, resultSize + 1).foldLeft((BitSet.empty, 1)) {
        case ((_, midElem), size) =>
          val newResult = solve(size, midElem)
          val newMidElem = newResult.drop(newResult.size / 2).head
          (newResult, newMidElem)
      }

    result.mkString
  }

}
