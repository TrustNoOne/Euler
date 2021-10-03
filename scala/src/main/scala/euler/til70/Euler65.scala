package euler
package til70

import euler._

object Euler65 extends EulerProblem {

  //1,2,1,1,4,1,...,1,2k,1,...
  def eContFractIterator = new Iterator[Int] {
    var (i, k) = (1, 0)
    val hasNext = true

    def next(): Int = {
      i = i + 1
      if (i % 3 == 0) {
        k = k + 1
        2 * k
      } else 1
    }

  }

  override def result() = {
    //starting from the 2nd (3)
    val res =
      eContFractIterator.take(99).foldRight((BigInt(1), BigInt(0))) { case (x, (num, den)) => (x * num + den, num) }

    val hundredthConvNum = res._2 + 2 * res._1
    hundredthConvNum.toString.map(_.toString.toInt).sum
  }

}
