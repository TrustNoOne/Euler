import Euler._

object Euler31to40 {
  def main(args: Array[String]) {
    println("31: " + elapsed(Euler31.solve()))
    println("32: " + elapsed(Euler32.solve()))
    println("33: " + elapsed(Euler33.solve()))
    println("34: " + elapsed(Euler34.solve()))
    println("35: " + elapsed(Euler35.solve()))
    println("36: " + elapsed(Euler36.solve()))
    println("37: " + elapsed(Euler37.solve()))
    println("38: " + elapsed(Euler38.solve()))
    println("39: " + elapsed(Euler39.solve()))
    println("40: " + elapsed(Euler40.solve()))
  }
}

object Euler40 {
  def solve() = {
    val champernowne = Iterator.from(1).flatMap(toDigits(_, 10).reverse)
    champernowne.next() * champernowne.drop(8).next() * champernowne.drop(89).next() *
      champernowne.drop(899).next() * champernowne.drop(8999).next() * champernowne.drop(89999).next() *
      champernowne.drop(899999).next()
  }
}

object Euler39 {
  /* a = m^2-n^2 b=2mn c = m^2+n^2
   * a+b+c = 2*m^2 +2mn = 2m(m+n)
   * 2m(m+n) <= 1000 
   */
  def solve() = {
    val primitiveTriples = for {
      n <- 1 to 999
      m <- n + 1 to 1000 by 2
      if 2 * m * (m + n) <= 1000
      if (2 to m / 2) forall { x => m % x > 0 || n % x > 0 }
    } yield (m * m - n * n, 2 * m * n, m * m + n * n)

    val allTriples = primitiveTriples flatMap { t =>
      def recursion(i: Int, l: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
        if (i * (t._1 + t._2 + t._3) > 1000) l
        else recursion(i + 1, (i * t._1, i * t._2, i * t._3) :: l)
      }
      recursion(1, Nil)
    }

    allTriples.map(t => t._1 + t._2 + t._3).groupBy(identity).mapValues(_.size).maxBy(_._2)
  }

}

object Euler38 {
  def isConcProdPandigital(n: Int): Option[Int] = {
    def calcPanRec(i: Int, res: Seq[Int]): Seq[Int] = res match {
      case x if x.length >= 9 => x
      case x => calcPanRec(i + 1, toDigits(n * i) ++: x)
    }

    val digits = calcPanRec(1, Nil)
    val isPandigital = digits.length == 9 && !digits.contains(0) && digits.toSet.size == 9
    if (isPandigital) Some(fromDigits(digits)) else None
  }

  def solve() = {
    val pandigits = (1 to 9999).par map isConcProdPandigital filter { _.isDefined } map { _.get }
    pandigits.toList.sorted.last
  }
}

object Euler37 {
  def solve() = {
    Stream.from(9, 2).filter { n =>
      val lr = toDigits(n).scanLeft((1, 0))((s, x) => (s._1 * 10, s._2 + s._1 * x)).map(_._2).drop(1)
      val rl = toDigits(n).reverse.scanLeft(0)((s, x) => s * 10 + x).drop(1)
      (lr forall isPrime) && (rl forall isPrime)
    } take (11) sum
  }
}

object Euler36 {
  def isPalindrome[T](xs: Seq[T]) =
    (xs zip xs.reverse) take (xs.size / 2) forall (x => x._1 == x._2)

  def solve() = {
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

  def solve() = Range(3, 1000000, 2).filter(rotations(_) forall isPrime).size + 1 //2
}

object Euler34 {
  def solve() = {
    val factorials = 0 to 9 map (n => fact(n).toInt)
    (1 to factorials.sum) filter { n =>
      val digits = toDigits(n)
      digits.size > 1 && n == digits.map(factorials).sum
    } sum
  }
}

object Euler33 {
  def solve() = {
    val curiousFrac = for ( // nx/xd = n/d already 4, no need to check xn/dx etc
      x <- 1 to 9; n <- 0 to 9; d <- 1 to 9 if (n < x || n == x && x < d) &&
        (n * 10 + x) * d == n * (x * 10 + d)
    ) yield ((n * 10 + x), (x * 10 + d))
    val curiousProd = curiousFrac.reduceLeft((prod, x) => (prod._1 * x._1, prod._2 * x._2))
    curiousProd._2 / curiousProd._1 // it's 100, no need to simplify
  }
}

object Euler32 {
  def solve() = {
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

  def solve() = subprob(0, 200)
}

