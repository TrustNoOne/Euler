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

  def fromDigits(xs: Seq[Int]): Int =
    xs.foldLeft((1, 0))((a, b) => (a._1 * 10, a._2 + a._1 * b))._2

  def isPrime(n: Long) = {
    2L to math.sqrt(n).toLong forall (n % _ > 0)
  }

  def isPrime(n: Int) = {
    2 to math.sqrt(n).toInt forall (n % _ > 0)
  }

}