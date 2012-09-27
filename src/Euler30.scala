import Euler._

object Resolve30 {
  def main(args: Array[String]) {
    println("21: " + elapsed(Euler21 resolve))
    println("22: " + elapsed(Euler22 resolve))
    println("23: " + elapsed(Euler23 resolve))
    println("24: " + elapsed(Euler24 resolve))
    println("25: " + elapsed(Euler25 resolve))
    println("26: " + elapsed(Euler26 resolve))
    println("27: " + elapsed(Euler27 resolve))
    println("28: " + elapsed(Euler28 resolve))
    println("29: " + elapsed(Euler29 resolve))
    println("30: " + elapsed(Euler30 resolve))
  }
}

object Euler30 {
  def resolve = { // 9^5*6 = 354294 (6 digits)
    val nums = for (d1 <- 0 to 9; d2 <- 0 to 9; d3 <- 0 to 9; d4 <- 0 to 9; d5 <- 0 to 9; d6 <- 0 to 9)
      yield (d1 + d2 * 10 + d3 * 100 + d4 * 1000 + d5 * 10000 + d6 * 100000,
      d1 * d1 * d1 * d1 * d1 + d2 * d2 * d2 * d2 * d2 + d3 * d3 * d3 * d3 * d3 +
      d4 * d4 * d4 * d4 * d4 + d5 * d5 * d5 * d5 * d5 + d6 * d6 * d6 * d6 * d6)
    nums.filter(x => x._1 == x._2).map(_._1).filter(n => n != 0 && n != 1).sum
  }
}

object Euler29 {
  def resolve = {
    val pows = for (a <- 2 to 100; b <- 2 to 100) yield math.pow(a, b)
    Set(pows: _*).size
  }
}

object Euler28 {
  def corners: Stream[(Int, Int, Int, Int)] = (0, 0, 0, 1) #:: Stream.from(1).zip(corners).map { n =>
    (n._2._4 + 2 * n._1, n._2._4 + 4 * n._1, n._2._4 + 6 * n._1, n._2._4 + 8 * n._1)
  }

  def resolve = corners.take(501).foldLeft(0)((sum, c) => sum + c._1 + c._2 + c._3 + c._4)
}

object Euler27 {
  def isPrime(n: Int) = n > 1 && (2L to math.sqrt(n).toInt forall (n % _ > 0))

  def resolve = {
    val numPrimes = for (a <- Range(-999, 999).view; b <- Range(-999, 999).view)
      yield (Stream.from(0).map(n => n * n + a * n + b).takeWhile(isPrime).size, a, b)
    val maxNumPrimes = numPrimes.maxBy(_._1)
    maxNumPrimes._2 * maxNumPrimes._3
  }
}

object Euler26 {
  def decimals(n: Int, d: Int): Stream[(Int, Int)] = {
    if (n == 0) Stream.empty
    else ((n % d * 10 / d), n % d) #:: decimals(n % d * 10, d)
  }

  def repeatingDecimals(n: Int, d: Int): List[Int] = {
    var remainders: List[Int] = Nil
    var remainIdx: Int = 0
    val repeating = decimals(n, d) takeWhile { x =>
      remainIdx = remainders.indexOf(x._2)
      remainders ::= x._2
      remainIdx < 0
    }
    repeating.map(_._1).toList.takeRight(remainIdx + 1)
  }

  def resolve = (2 until 1000).map(repeatingDecimals(1, _).size).zipWithIndex.max._2 + 2

}

object Euler25 {
  def fibWithDigits(numDigits: Int) = {
    def fibWithDigitsTr(curr: BigInt, prev: BigInt, idx: BigInt): BigInt = curr match {
      case x if x.toString.length >= numDigits => idx + 1
      case _ => fibWithDigitsTr(curr + prev, curr, idx + 1)
    }
    fibWithDigitsTr(1, 1, 1)
  }

  def resolve = fibWithDigits(1000)
}

object Euler24 {
  def fact(n: Int, acc: Int = 1): Int = {
    if (n == 1) acc
    else fact(n - 1, acc * n)
  }

  def res(_idx: Int, numElems: Int) = {
    import scala.collection.mutable.ListBuffer
    val idx = _idx - 1 /*1-based*/
    var remain: Int = idx
    var elems = ListBuffer.range(0, numElems)
    (0 until numElems) map { i =>
      if (remain == 0) elems.remove(0)
      else {
        val currFact = fact(elems.size - 1)
        val nextDigitIdx = remain / currFact
        remain %= currFact
        elems.remove(nextDigitIdx)
      }
    }
  }

  def resolve = res(1000000, 10).mkString
}

object Euler23 {
  import Euler21.divisors
  def resolve = {
    val abundants = (1 to 28123).filter(i => divisors(i).sum > i)
    val sums = collection.mutable.Set.empty[Int]
    for (x <- abundants; y <- abundants) if (x + y <= 28123) sums += x + y

    (1 to 28123).filterNot(sums.contains).sum
  }
}

object Euler22 {
  def alphabeticalValue(s: String) = s.foldLeft(0)(_ + _.toInt - 64)
  def resolve = {
    val file = io.Source.fromFile("names.txt")
    val list = file.mkString.replaceAll("\"", "").split(",")
    file.close
    list.sorted.zipWithIndex.foldLeft(0) {
      (acc, nameIdx) => acc + alphabeticalValue(nameIdx._1) * (nameIdx._2 + 1)
    }
  }
}

object Euler21 {
  def divisors(n: Int) = (1 to n / 2) filter { n % _ == 0 }

  def resolve = {
    val sumOfDivisors = (2 to 10000).par map { divisors(_).sum }
    val amicable = sumOfDivisors.zipWithIndex.filter {
      case (v, i) =>
        v > 1 && v <= 10000 && v != i + 2 && i + 2 == sumOfDivisors(v - 2)
    }.unzip._1
    amicable.sum
  }
}
