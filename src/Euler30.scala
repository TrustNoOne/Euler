object Resolve30 {
  def main(args: Array[String]) {
    println("21: " + (Euler21 resolve))
    //    println("22: " + (Euler22 resolve))
    //    println("23: " + (Euler23 resolve))
    //    println("24: " + (Euler24 resolve))
    //    println("25: " + (Euler25 resolve))
    //    println("26: " + (Euler26 resolve))
    //    println("27: " + (Euler27 resolve))
    //    println("28: " + (Euler28 resolve))
    //    println("29: " + (Euler29 resolve))
    //    println("30: " + (Euler30 resolve))
  }
}

object Euler21 {
  def divisors(n: Int) = (1 to n / 2) filter { n % _ == 0 }
  
  def resolve = {
    val sumOfDivisors = (2 to 10000).par map { divisors(_).sum }
    val amicable = sumOfDivisors.zipWithIndex.filter {
      case (v, i) =>
        v > 1 && v <= 10000 && v != i+2 && i + 2 == sumOfDivisors(v - 2)
    }.unzip._1
    amicable.sum
  }
}
