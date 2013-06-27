object Euler {
  def resource(fileName: String) =
    io.Source.fromURL(getClass.getResource("./" + fileName))

  def withResource(fileName: String)(block: io.Source => Any) = {
    val source = resource(fileName)
    try {
      block(source)
    } finally {
      source.close()
    }
  }

  def elapsed(t: => Any) = {
    val t0 = System.currentTimeMillis()
    t.toString + " in " + (System.currentTimeMillis() - t0) + " ms"
  }

  def fact(n: Int): BigInt = {
    def f(n: Int, acc: BigInt = 1): BigInt =
      if (n <= 1) acc else f(n - 1, acc * n)
    f(n)
  }

  def combinations(n: Int, k: Int) = {
    (BigInt(n - k + 1) to BigInt(n)).product / (BigInt(2) to BigInt(k)).product
  }

  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A]))((x, y) => for (a <- x.view; b <- y) yield a :+ b)

  def toDigits(n: Int): Seq[Int] =
    if (n < 10) Seq(n) else (n % 10) +: toDigits(n / 10)

  def toBinaryDigits(n: Int): Seq[Int] =
    if (n < 2) Seq(n) else (n % 2) +: toDigits(n / 2)

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

  //  def isPerfectSquare(n: Long) = math.sqrt(n) % 1 == 0
  //  def isPerfectSquare(n: Int) = math.sqrt(n) % 1 == 0
  def isTriangular(n: Int) = isPerfectSquare(8 * n + 1)
  def isTriangular(n: Long) = isPerfectSquare(8 * n + 1)
  def isPentagonal(n: Int) = math.sqrt(24 * n + 1) % 6 == 5
  def isPentagonal(n: Long) = math.sqrt(24 * n + 1) % 6 == 5

  object LongIterator {
    def from(start: Long, step: Long): Iterator[Long] = new Iterator[Long] {
      private var i = start
      def hasNext: Boolean = true
      def next(): Long = { val result = i; i += step; result }
    }
    def from(start: Long): Iterator[Long] = from(start, 1)
  }

  implicit class ExtInt(n: Int) {
    def isPrime = Euler.isPrime(n)
    def toDigits = Euler.toDigits(n)
    def toBinaryDigits = Euler.toBinaryDigits(n)
    def fact = Euler.fact(n)
    def isPerfectSquare = Euler.isPerfectSquare(n)
    def isTriangular = Euler.isTriangular(n)
  }

  implicit class ExtLong(n: Long) {
    def isPrime = Euler.isPrime(n)
    def toDigits = Euler.toDigits(n)
    def isPerfectSquare = Euler.isPerfectSquare(n)
    def isTriangular = Euler.isTriangular(n)
  }

}