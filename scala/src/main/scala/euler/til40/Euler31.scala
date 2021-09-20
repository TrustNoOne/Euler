package euler
package til40

object Euler31 extends EulerProblem {
  val Coins = Seq(200, 100, 50, 20, 10, 5, 2, 1)

  def subprob(coinIdx: Int, target: Int): Int = {
    if (coinIdx == 7) 1
    else {
      val coinVal = Coins(coinIdx)
      (0 to target / coinVal)
        .map(n => subprob(coinIdx + 1, target - n * coinVal))
        .sum
    }
  }

  override def result = subprob(0, 200)
}
