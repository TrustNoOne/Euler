import Euler._
import scala.math.BigInt

object Euler51to60 {
  def main(args: Array[String]) {
    //    println("51: " + elapsed(Euler51.solve()))
    println("52: " + elapsed(Euler52.solve()))
    //    println("53: " + elapsed(Euler53.solve()))
    //    println("54: " + elapsed(Euler54.solve()))
    //    println("55: " + elapsed(Euler55.solve()))
    //    println("56: " + elapsed(Euler56.solve()))
    //    println("57: " + elapsed(Euler57.solve()))
    //    println("58: " + elapsed(Euler58.solve()))
    //    println("59: " + elapsed(Euler59.solve()))
    //    println("60: " + elapsed(Euler60.solve()))
  }
}

object Euler52 {
  def solve() = {
    val res = Iterator.from(1) dropWhile { x =>
      val sortedDigits = toDigits(x).sorted
      (2 to 6).exists { n => sortedDigits != toDigits(n * x).sorted }
    }
    res.next()
  }
}

object Euler51 {
  /**
   * primePermutations(123, 2) returns all prime permutations of 123xx with xx == 00,11,..,99
   */
  def primePermutations(n: Int, addedDigitCnt: Int) = {
    val digits = toDigits(n) ++ List.fill(addedDigitCnt)(-1)
    digits.permutations.map { x =>
      (0 to 9).map { d =>
        x.map { case -1 => d; case a => a }
      } map fromDigits filter isPrime
    } filterNot { _.isEmpty }
  }
  def solve() = {
    val res = Stream.from(100) flatMap { primePermutations(_, 3) } dropWhile (xs => xs.size < 8 || xs.head < 100000)
    res.head.head
  }
}


