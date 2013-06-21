import Euler._

object Euler41to50 {
  def main(args: Array[String]) {
    //    println("41: " + elapsed(Euler41.solve()))
    //    println("42: " + elapsed(Euler42.solve()))
    //    println("43: " + elapsed(Euler43.solve()))
    println("44: " + elapsed(Euler44.solve()))
    //    println("45: " + elapsed(Euler45.solve()))
    //    println("46: " + elapsed(Euler46.solve()))
    //    println("47: " + elapsed(Euler47.solve()))
    //    println("48: " + elapsed(Euler48.solve()))
    //    println("49: " + elapsed(Euler49.solve()))
    //    println("50: " + elapsed(Euler50.solve()))
  }
}

object Euler44 {
  val limit = 10000
  val pentagonals = Array(1 to limit: _*) map { n => n * (3 * n - 1) / 2 }
  def isPentagonal(n: Int) = {
    java.util.Arrays.binarySearch(pentagonals, n) >= 0
    //        math.sqrt(24 * n + 1) % 6 == 5
  }
  def solve() = {
    for {
      n <- (0 until limit).par
      m <- (n + 1 until limit)
      if (isPentagonal(pentagonals(m) + pentagonals(n)))
      if (isPentagonal(pentagonals(m) - pentagonals(n)))
    } yield pentagonals(m) - pentagonals(n)
  }
}

object Euler43 {
  def subStringDivCheck(xs: Seq[Long]) = {
    fromDigits(xs.slice(6, 9)) % 2 == 0 &&
      fromDigits(xs.slice(5, 8)) % 3 == 0 &&
      fromDigits(xs.slice(4, 7)) % 5 == 0 &&
      fromDigits(xs.slice(3, 6)) % 7 == 0 &&
      fromDigits(xs.slice(2, 5)) % 11 == 0 &&
      fromDigits(xs.slice(1, 4)) % 13 == 0 &&
      fromDigits(xs.slice(0, 3)) % 17 == 0
  }
  def solve() = {
    List(0L, 1, 2, 3, 4, 5, 6, 7, 8, 9).permutations.filter(subStringDivCheck).map(fromDigits).sum
  }
}

object Euler42 {
  def wordvalue(w: String) = w.foldLeft(0)(_ + _.toInt - 64)
  def solve() = {
    val file = io.Source.fromFile("words.txt")
    val words = file.mkString.replaceAll("\"", "").split(",")
    file.close
    words.map(wordvalue).filter(isTriangular).length
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

