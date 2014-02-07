package object euler {
  //Implicit conversion and common methods
  def fact(n: Int): BigInt = {
    def f(n: Int, acc: BigInt = 1): BigInt =
      if (n <= 1) acc else f(n - 1, acc * n)
    f(n)
  }

  def combinations(n: Int, k: Int) = {
    (BigInt(n - k + 1) to BigInt(n)).product / (BigInt(2) to BigInt(k)).product
  }

  def toDigits(n: Int): Seq[Int] =
    if (n < 10) Seq(n) else (n % 10) +: toDigits(n / 10)
  def toDigits(n: Long): Seq[Long] =
    if (n < 10) Seq(n) else (n % 10) +: toDigits(n / 10)

  def fromDigits(xs: Seq[Int]): Int =
    xs.foldLeft((1, 0))((a, b) => (a._1 * 10, a._2 + a._1 * b))._2
  def fromDigits(xs: Seq[Long]): Long =
    xs.foldLeft((1L, 0L))((a, b) => (a._1 * 10, a._2 + a._1 * b))._2

  def isPrime(n: Long) = {
    if (n < 2) false
    else 2L to math.sqrt(n).toLong forall (n % _ > 0)
  }

  def isPrime(n: Int) = {
    if (n < 2) false
    else 2 to math.sqrt(n).toInt forall (n % _ > 0)
  }

  val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
  def primesIterator = Iterator(2) ++ Iterator.from(3, 2).filter(isPrime)

  def primeFactors(n: Long): Seq[Int] = {
    val primes = euler.primes.iterator
    @annotation.tailrec
    def loop(n: Long, factors: List[Int], p: Int): List[Int] = n match {
      case 1 => factors
      case n if n % p == 0 => loop(n / p, p :: factors, p)
      case _ => loop(n, factors, primes.next)
    }

    loop(n, List[Int](), primes.next)
  }

  def isPerfectSquare(n: Long) = {
    // http://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer
    if (n <= 0) false else n & 0x3F match {
      case 0x00 | 0x01 | 0x04 | 0x09 | 0x10 | 0x11
        | 0x19 | 0x21 | 0x24 | 0x29 | 0x31 | 0x39 =>
        math.sqrt(n) % 1 == 0
      case _ => false
    }
  }
  def isPerfectSquare(n: Int): Boolean = isPerfectSquare(n.toLong)

  def isPerfectCube(n: Long): Boolean = {
    val x = math.cbrt(n)
    x == Math.rint(x)
  }
  def isPerfectCube(n: Int): Boolean = isPerfectCube(n.toLong)

  //  def isPerfectSquare(n: Long) = math.sqrt(n) % 1 == 0
  //  def isPerfectSquare(n: Int) = math.sqrt(n) % 1 == 0
  def isTriangular(n: Int) = isPerfectSquare(8 * n + 1)
  def isTriangular(n: Long) = isPerfectSquare(8 * n + 1)
  def isPentagonal(n: Int) = math.sqrt(24 * n + 1) % 6 == 5
  def isPentagonal(n: Long) = math.sqrt(24 * n + 1) % 6 == 5
  def isHexagonal(n: Int) = {
    val x = (math.sqrt(8 * n + 1) + 1) / 4
    x == Math.rint(x)
  }
  def isHeptagonal(n: Int) = {
    val x = (math.sqrt(40 * n + 9) + 3) / 10
    x == Math.rint(x)
  }
  def isNgonal(s: Int, n: Int) = {
    val x = (math.sqrt(n * (8 * s - 16) + math.pow(s - 4, 2)) + s - 4) / (2 * s - 4)
    x == Math.rint(x)
  }

  def isPalindrome(n: Int): Boolean = isPalindrome(n.toString)
  def isPalindrome(n: Long): Boolean = isPalindrome(n.toString)
  def isPalindrome(s: String) = (s, s.reverse).zipped forall (_ == _)

  implicit class ExtInt(val n: Int) extends AnyVal {
    def isPrime = euler.isPrime(n)
    def toDigits = euler.toDigits(n)
    def fact = euler.fact(n)
    def isPerfectSquare = euler.isPerfectSquare(n)
    def isTriangular = euler.isTriangular(n)
    def isPentagonal = euler.isPentagonal(n)
    def isPalindrome = euler.isPalindrome(n)
  }

  implicit class ExtLong(val n: Long) extends AnyVal {
    def isPrime = euler.isPrime(n)
    def toDigits = euler.toDigits(n)
    def isPerfectSquare = euler.isPerfectSquare(n)
    def isTriangular = euler.isTriangular(n)
    def isPentagonal = euler.isPentagonal(n)
    def isPalindrome = euler.isPalindrome(n)
  }

  object LongIterator {
    def from(start: Long, step: Long): Iterator[Long] = new Iterator[Long] {
      private var i = start
      def hasNext: Boolean = true
      def next(): Long = { val result = i; i += step; result }
    }
    def from(start: Long): Iterator[Long] = from(start, 1)
  }
}