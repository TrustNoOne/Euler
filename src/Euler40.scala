import Euler._

object Resolve40 {
  def main(args: Array[String]) {
    println("31: " + elapsed(Euler31 resolve))
    //    println("32: " + (Euler32 resolve))
    //    println("33: " + (Euler33 resolve))
    //    println("34: " + (Euler34 resolve))
    //    println("35: " + (Euler35 resolve))
    //    println("36: " + (Euler36 resolve))
    //    println("37: " + (Euler37 resolve))
    //    println("38: " + (Euler38 resolve))
    //    println("39: " + (Euler39 resolve))
    //    println("40: " + (Euler40 resolve))
  }
}

object Euler31 {
  val Coins = Seq(200, 100, 50, 20, 10, 5, 2, 1)

  def subprob(coinIdx: Int, target: Int): Int = {
    if (coinIdx == 7) 1 else {
      val coinVal = Coins(coinIdx)
      (0 to target / coinVal).map(n => subprob(coinIdx + 1, target - n * coinVal)).sum
    }
  }

  def resolve = subprob(0, 200)
}

