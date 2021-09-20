package euler
package til100

object Euler93 extends EulerProblem {

  val Operations =
    List[(Double, Double) => Double](
      _ + _,
      _ - _,
      _ * _,
      (x, y) => if (y != 0) x / y else Double.MinValue)

  override def result = {
    val longestConsecutives = for {
      a <- (0 to 9).view
      b <- a + 1 to 9
      c <- b + 1 to 9
      d <- c + 1 to 9
    } yield
      (longestConsecutive(a.toDouble, b.toDouble, c.toDouble, d.toDouble),
       s"$a$b$c$d")

    longestConsecutives.maxBy(_._1)._2
  }

  def longestConsecutive(a: Double, b: Double, c: Double, d: Double) = {
    val all = for {
      xs <- List(a, b, c, d).permutations
      op1 <- Operations
      op2 <- Operations
      op3 <- Operations
      // [op3 [op2 [op1 a b] c] d] or op3[[op1 a b][op2 c d]]
      res <- Seq(op3(op2(op1(xs(0), xs(1)), xs(2)), xs(3)),
                 op3(op1(xs(0), xs(1)), op2(xs(2), xs(3))))
    } yield res

    def loop(xs: List[Double], start: Double = 1): Double = xs match {
      case h :: t if h == start => loop(t, h + 1)
      case _                    => start - 1
    }

    val targets =
      all.filter(x => x > 0 && x == math.rint(x)).toSet.toList.sorted
    loop(targets).toInt
  }

}
