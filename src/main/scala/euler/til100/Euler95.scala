package euler
package til100

object Euler95 extends EulerProblem {

  val Max = 1000000
  val chains = Array.fill(Max + 1)(1)

  override def result = {
    (2 to Max).foreach(x => (2 * x to Max by x).foreach(chains(_) += x))

    (2 to Max).foldLeft((Max + 1, -1)) {
      case ((res, maxLen), x) =>
        val currChainLen = chainLen(x)
        if (currChainLen > maxLen) (x, currChainLen)
        else if (currChainLen == maxLen && x < res) (x, currChainLen)
        else (res, maxLen)
    }._1
  }


  def chainLen(start: Int) = {
    def loop(x: Int, len: Int, visited: Set[Int]): Int = {
      val next = chains(x)
      if (next == start) len
      else if (next > 1000000 || visited.contains(next)) -1
      else loop(next, len + 1, visited + x)
    }

    loop(start, 1, Set(1))
  }
}


