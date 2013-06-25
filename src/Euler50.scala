import Euler._
import scala.math.BigInt

object Euler41to50 {
  def main(args: Array[String]) {
    println("41: " + elapsed(Euler41.solve()))
    println("42: " + elapsed(Euler42.solve()))
    println("43: " + elapsed(Euler43.solve()))
    println("44: " + elapsed(Euler44.solve()))
    println("45: " + elapsed(Euler45.solve()))
    println("46: " + elapsed(Euler46.solve()))
    println("47: " + elapsed(Euler47.solve()))
    println("48: " + elapsed(Euler48.solve()))
    println("49: " + elapsed(Euler49.solve()))
    println("50: " + elapsed(Euler50.solve()))
  }
}

object Euler50 {
  val primes = 2 +: Range(3, 5000, 2).filter(isPrime)
  def solve() = {
    val primeSumSeqs = (2 to primes.size).view.flatMap(primes.sliding(_)).filter { xs =>
      val sum = xs.sum;
      sum.isPrime && sum < 1000000
    }
    primeSumSeqs.maxBy(_.size).sum
  }
}

object Euler49 {
  val fourDigitPrimes = Range(1001, 9999, 2).filter(isPrime)
  def solve() = {
    val primesWithAtLeastThreePrimePermutations =
      fourDigitPrimes.groupBy(toDigits(_).sorted).values.filter(_.size >= 3)
    /* find all the "distances" between the primes in the set, and filter only
     * the sets that have three primes with the same distance (difference)
     */
    primesWithAtLeastThreePrimePermutations.flatMap { xs => // unreadable?
      xs.combinations(2).toList.groupBy(x => x(1) - x(0)) //group by difference
        .values.map(_.flatten.toSet).filter(_.size == 3).map(_.mkString)
    }
  }
}

object Euler48 {
  def solve() = (1 to 1000).map(n => BigInt(n).pow(n)).sum % 10000000000L
}

object Euler47 {
  val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
  def factorCount(n: Int) = primes.takeWhile(_ <= n / 2).count(n % _ == 0)
  def factorCount2(n: Int) = { // 10 times faster
    def loop(n: Int, cnt: Int, primes: Stream[Int]): Int = n match {
      case x if primes.head > x => cnt
      case x if x % primes.head == 0 => loop(x / primes.head, cnt + 1, primes.tail)
      case _ => loop(n, cnt, primes.tail)
    }
    loop(n, 0, primes)
  }
  def solve() = {
    val res = Iterator.from(2 * 3 * 5 * 7).dropWhile { n =>
      (n to n + 3).exists(factorCount2(_) != 4)
    }
    res.next
  }
}

object Euler46 {
  val primes = Stream.from(3, 2).filter(isPrime) // 2 is not needed
  val oddComposites = Stream.from(9, 2).filterNot(isPrime)
  def isGoldbach(n: Int) = {
    primes.takeWhile(n - _ >= 2).map(p => (n - p) / 2)
      .filter(isPerfectSquare).size > 0
  }
  def solve() = oddComposites.dropWhile(isGoldbach).head
}

object Euler45 {
  def solve() = {
    val hexagonals = LongIterator.from(0L) map { n => n * (2 * n - 1) }
    hexagonals.filter(n => isTriangular(n) && isPentagonal(n)).take(3).toList.last
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

