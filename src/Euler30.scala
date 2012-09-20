object Resolve30 {
  def main(args: Array[String]) {
    //    println("21: " + (Euler21 resolve))
    //    println("22: " + (Euler22 resolve))
    //    println("23: " + (Euler23 resolve))
    //    println("24: " + (Euler24 resolve))
    println("25: " + (Euler25 resolve))
    //    println("26: " + (Euler26 resolve))
    //    println("27: " + (Euler27 resolve))
    //    println("28: " + (Euler28 resolve))
    //    println("29: " + (Euler29 resolve))
    //    println("30: " + (Euler30 resolve))
  }
}

object Euler25 {
  def fibWithDigits(numDigits: Int) = {
    def fibWithDigitsTr(curr: BigInt, prev: BigInt, idx:BigInt): BigInt = curr match {
      case x if x.toString.length >= numDigits => idx+1
      case _ => fibWithDigitsTr(curr + prev, curr, idx+1)
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
