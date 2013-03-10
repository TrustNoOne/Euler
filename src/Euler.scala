object Euler {
  def elapsed(t: => Any) = {
    val t0 = System.currentTimeMillis()
    t.toString + " in " + (System.currentTimeMillis() - t0) + " ms"
  }

  def fact(n: Int): BigInt = {
    def f(n: Int, acc: BigInt = 1): BigInt =
      if (n <= 1) acc else f(n - 1, acc * n)
    f(n)
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

  def isPerfectSquare(n: Long) = math.sqrt(n) % 1 == 0
  def isPerfectSquare(n: Int) = math.sqrt(n) % 1 == 0
  def isTriangular(n: Int) = isPerfectSquare(8 * n + 1)
  def isTriangular(n: Long) = isPerfectSquare(8 * n + 1)

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