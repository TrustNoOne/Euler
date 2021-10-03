package euler
package til100

import scala.collection.parallel.CollectionConverters._

object Euler92 extends EulerProblem {

  override def result() = {
    def chainEnd(x: Int, found1: Boolean = false, found89: Boolean = false): Int = x match {
      case 1 if found1 => 1
      case 89 if found89 => 89
      case _ => chainEnd(next(x), found1 || x == 1, found89 || x == 89)
    }

    (1 until 10000000).par count (chainEnd(_) == 89)
  }

  // next in the square digit chain
  def next(x: Int): Int = x.toDigits.fold(0)((sum, d) => sum + d * d)

}
