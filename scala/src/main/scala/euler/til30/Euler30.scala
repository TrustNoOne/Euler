package euler
package til30

object Euler30 extends EulerProblem {
  override def result() = { // 9^5*6 = 354294 (6 digits)
    val nums = for {
      d1 <- 0 to 9
      d2 <- 0 to 9
      d3 <- 0 to 9
      d4 <- 0 to 9
      d5 <- 0 to 9
      d6 <- 0 to 9
    } yield (
      d1 + d2 * 10 + d3 * 100 + d4 * 1000 + d5 * 10000 + d6 * 100000,
      d1 * d1 * d1 * d1 * d1 + d2 * d2 * d2 * d2 * d2 + d3 * d3 * d3 * d3 * d3 +
        d4 * d4 * d4 * d4 * d4 + d5 * d5 * d5 * d5 * d5 + d6 * d6 * d6 * d6 * d6,
    )
    nums.filter(x => x._1 == x._2).map(_._1).filter(n => n != 0 && n != 1).sum
  }
}
