package euler
package til90

object Euler88 extends EulerProblem {

  // produces tuples (listOfInts, prod, sum)
  // http://stackoverflow.com/questions/25823177/create-a-scala-function-that-generates-ordered-list-of-integers-of-length-n
  def intCombinations(n: Int, limit: Int, k: Int = 2, currProd: Int = 1): Seq[(List[Int], Int, Int)] = {
    (k to limit * 2 / currProd).flatMap {
      case x if n > 1 =>
        intCombinations(n - 1, limit, x, currProd * x).map(t => (x :: t._1, t._2 * x, t._3 + x))
      case x => Seq((List(x), x, x))
    }
  }

  override def result() = {
    // 2^15 = 32768>24000
    val kMax = 12000
    val minProdSumNumbers =
      (2 to 15).view
        .flatMap(intCombinations(_, kMax))
        .foldLeft(Map[Int, Int]()) { case (map, (list, prod, sum)) =>
          val k = list.length + prod - sum

          if (k > kMax) map
          else
            map.get(k).fold(map.updated(k, prod)) { value => if (value < prod) map else map.updated(k, prod) }
        }

    minProdSumNumbers.values.toSet.sum
  }

}
