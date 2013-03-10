import Euler._

object Euler41to50 {
  def main(args: Array[String]) {
    println("41: " + elapsed(Euler41.solve()))
    //    println("42: " + elapsed(Euler42.solve()))
    //    println("43: " + elapsed(Euler43.solve()))
    //    println("44: " + elapsed(Euler44.solve()))
    //    println("45: " + elapsed(Euler45.solve()))
    //    println("46: " + elapsed(Euler46.solve()))
    //    println("47: " + elapsed(Euler47.solve()))
    //    println("48: " + elapsed(Euler48.solve()))
    //    println("49: " + elapsed(Euler49.solve()))
    //    println("50: " + elapsed(Euler50.solve()))
  }
}

object Euler41 {
  def solve() = {
    val digits = List(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)
    val pandigitalPrimes = (1 to 9) flatMap { n =>
      digits.take(n).permutations.filter(fromDigits(_).isPrime).map(fromDigits)
    } 
    pandigitalPrimes.sorted.last
  }
}

