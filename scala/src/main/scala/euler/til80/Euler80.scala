package euler
package til80

object Euler80 extends EulerProblem {

  override def result() = {
    val res = (2 to 99) filterNot isPerfectSquare map { sqrDigits(_, 100).sum }
    res.sum
  }

  def sqrDigits(x: Int, numDigits: Int): Seq[Int] = {
    val pairsIt = pairsIterator(x)
    @annotation.tailrec
    def loop(c: BigInt, p: BigInt, res: Seq[Int]): Seq[Int] =
      if (res.size == numDigits) res
      else {
        //Determine the greatest digit x such that x(20p + x) <= c
        val x = Iterator
          .from(0)
          .find { x =>
            val nextX = x + 1
            nextX * (20 * p + nextX) > c
          }
          .get
        val y = x * (20 * p + x)

        val nextC = (c - y) * 100 + pairsIt.next()
        val nextP = p * 10 + x

        loop(nextC, nextP, res :+ x)
      }

    loop(pairsIt.next(), 0, Vector())
  }

  /**
   * iterator of pairs of digits. E.g. for 12345 -> Seq(1,23,45)
   */
  def pairsIterator(x: Int) = {
    val xStr = x.toString
    val xDigits =
      if (xStr.size % 2 == 0) xStr.map(_.toString.toInt)
      else ('0' +: xStr).map(_.toString.toInt)

    xDigits.grouped(2).map(xs => xs(0) * 10 + xs(1)) ++ Iterator.continually(0)
  }

}
