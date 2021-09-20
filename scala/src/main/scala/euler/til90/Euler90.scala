package euler
package til90

object Euler90 extends EulerProblem {
  val Squares = Set(1, 4, 9, 16, 25, 36, 49, 64, 81)

  override def result = {
    // all possible distinct cubes, 21945
    (0 to 9).combinations(6).toList.combinations(2) count { cubes =>
      displayAllSquares(cubes(0), cubes(1))
    }
  }

  def displayAllSquares(cube1: Seq[Int], cube2: Seq[Int]): Boolean = {
    val displayableSquares = for {
      x <- cube1
      y <- cube2

      n <- // a 6 and 9 are interchangeable
      if (x == 6 || x == 9) List(60 + y, 90 + y, 10 * y + 6, 10 * y + 9)
      else if (y == 6 || y == 9) List(10 * x + 6, 10 * x + 9, 60 + x, 90 + x)
      else List(10 * x + y, 10 * y + x)
      if Squares.contains(n)
    } yield n

    displayableSquares.toSet.size == Squares.size
  }

}
