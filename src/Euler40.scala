import Euler._

object Euler31to40 {
  def main(args: Array[String]) {
    println("31: " + elapsed(Euler31.solve))
    //    println("32: " + (Euler32.solve))
    //    println("33: " + (Euler33.solve))
    //    println("34: " + (Euler34.solve))
    //    println("35: " + (Euler35.solve))
    //    println("36: " + (Euler36.solve))
    //    println("37: " + (Euler37.solve))
    //    println("38: " + (Euler38.solve))
    //    println("39: " + (Euler39.solve))
    //    println("40: " + (Euler40.solve))
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

  def solve = subprob(0, 200)
}

