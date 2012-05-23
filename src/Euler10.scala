object Resolve10 {
  def main(args: Array[String]) {
	  println(Euler10 resolve)
  }
}

object Euler10 {
  def resolve = 2L to 2000000L filter (Euler7.isPrime(_)) reduceLeft (_+_)
}

object Euler9 {
  import scala.math._
  def resolve = {
    var res = (0, 0, 0.0)
    for (a <- 2 to 999; b <- a + 1 to 999 if a + b < 1000 if a + b + sqrt(a * a + b * b) == 1000)
      yield (a, b, sqrt(a * a + b * b))
  }
}

object Euler8 {
  val number = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450""".replace("\n", "")

  def resolve = {
    (number.toList.sliding(5) map (
      _.foldLeft(1)((a, b) => a * (b - 48)))).toList.max
  }
}

object Euler7 {
  import scala.math.sqrt
  def isPrime(n: Long) = {
    2L to sqrt(n).toLong forall (n % _ > 0)
  }
  def buildPrime(count: Int, n: Int): Int = {
    if (isPrime(n) && count == 10001) n
    else if (isPrime(n)) buildPrime(count + 1, n + 1)
    else buildPrime(count, n + 1)
  }

  def resolve = {
    buildPrime(1, 2)
  }
}

object Euler6 {
  def resolve = {
    val n = 100
    val sum = (n * (n + 1)) / 2
    val squareOfSum = sum * sum
    val sumOfSquare = (1 to n).reduceLeft((a, b) => a + b * b)
    squareOfSum - sumOfSquare
  }
}

object Euler5 {
  def resolve = {
    val divisors = List(20, 19, 18, 17, 16, 15, 14, 13, 12, 11)
    def go(n: Int): Int = {
      if (divisors forall (n % _ == 0)) n
      else go(n + 1)
    }
    go(1)
  }
}

object Euler4 {
  def isPalindrome(n: Int) = (n toString (), n toString () reverse).zipped forall (_ == _)

  def resolve = {
    var max = (0, 0, 0)
    for (i <- 100 to 999; j <- i to 999) {
      val n = i * j
      if (isPalindrome(n) && n > max._1) max = (n, i, j)
    }
    max
  }
}