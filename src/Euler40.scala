import Euler._

object Euler31to40 {
  def main(args: Array[String]) {
    //    println("31: " + elapsed(Euler31.solve))
    //    println("32: " + elapsed(Euler32.solve))
    //    println("33: " + elapsed(Euler33.solve))
    //    println("34: " + elapsed(Euler34.solve))
    //    println("35: " + elapsed(Euler35.solve))
    println("36: " + elapsed(Euler36.solve))
    //    println("37: " + elapsed(Euler37.solve))
    //    println("38: " + elapsed(Euler38.solve))
    //    println("39: " + elapsed(Euler39.solve))
    //    println("40: " + elapsed(Euler40.solve))
  }
}

object Euler36 {
  def isPalindrome[T](xs: Seq[T]) =
    (xs zip xs.reverse) take (xs.size / 2) forall (x => x._1 == x._2)

  def solve = {
    1 to 1000000 filter { n =>
      isPalindrome(toDigits(n)) && isPalindrome(toDigits(n, 2))
    } sum
  }
}

object Euler35 {
  def rotations(n: Int) = {
    def rotStream(n: Int): Stream[Int] = {
      val digits = toDigits(n)
      val next = fromDigits(digits.tail :+ digits.head)
      next #:: rotStream(next)
    }
    n #:: (rotStream(n) takeWhile (_ != n))
  }

  def solve = Range(3, 1000000, 2).filter(rotations(_) forall isPrime).size + 1 //2
}

object Euler34 {
  def solve = {
    val factorials = 0 to 9 map (n => fact(n).toInt)
    (1 to factorials.sum) filter { n =>
      val digits = toDigits(n)
      digits.size > 1 && n == digits.map(factorials).sum
    } sum
  }
}

object Euler33 {
  def solve = {
    val curiousFrac = for ( // nx/xd = n/d already 4, no need to check xn/dx etc
      x <- 1 to 9; n <- 0 to 9; d <- 1 to 9 if (n < x || n == x && x < d) &&
        (n * 10 + x) * d == n * (x * 10 + d)
    ) yield ((n * 10 + x), (x * 10 + d))
    val curiousProd = curiousFrac.reduceLeft((prod, x) => (prod._1 * x._1, prod._2 * x._2))
    curiousProd._2 / curiousProd._1 // it's 100, no need to simplify
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

