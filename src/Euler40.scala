import Euler._

object Euler31to40 {
  def main(args: Array[String]) {
    //    println("31: " + elapsed(Euler31.solve))
    println("32: " + elapsed(Euler32.solve))
    //    println("33: " + elapsed(Euler33.solve))
    //    println("34: " + elapsed(Euler34.solve))
    //    println("35: " + elapsed(Euler35.solve))
    //    println("36: " + elapsed(Euler36.solve))
    //    println("37: " + elapsed(Euler37.solve))
    //    println("38: " + elapsed(Euler38.solve))
    //    println("39: " + elapsed(Euler39.solve))
    //    println("40: " + elapsed(Euler40.solve))
  }
}

object Euler32 {
  def solve = {
    val prods = for (a <- Range(1, 100).view; b <- Range(1, 10000).view if a * b < 9876)
      yield (a * b, "" + a + b + (a * b))
    prods.filter(x => x._2.length == 9 && !x._2.contains('0') && x._2.toSet.size == 9).map(_._1).toSet.sum
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

