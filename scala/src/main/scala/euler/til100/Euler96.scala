package euler
package til100

import euler.Utils.withResource

object Euler96 extends EulerProblem {

  val sudokus: Seq[Sudoku] = withResource("p096_sudoku.txt") { f =>
    f.getLines().grouped(10).map(x => new Sudoku(x.tail.map(_.map(_ - '0').toVector).toVector)).toList
  }

  def solve(s: Sudoku): Option[Sudoku] = s.firstEmptyCell match {
    case None => Some(s)
    case Some((i, j)) => s.validValues(i, j).view.flatMap(x => solve(s.updated(i, j, x))).headOption
  }

  override def result = {
    val solvedSudokus = sudokus flatMap solve
    solvedSudokus.map(_.firstThreeDigits).sum
  }

}


class Sudoku(grid: Seq[Seq[Int]]) {
  lazy val firstThreeDigits = grid(0)(0) * 100 + grid(0)(1) * 10 + grid(0)(2)

  lazy val firstEmptyCell: Option[(Int, Int)] = (for (i <- 0 until 9; j <- 0 until 9) yield (i, j)).collectFirst {
    case (i, j) if grid(i)(j) == 0 => (i, j)
  }

  def validValues(i: Int, j: Int): Set[Int] =
    (0 until 9).foldLeft(Set(1, 2, 3, 4, 5, 6, 7, 8, 9)) { (xs, c) =>
      xs - grid(i)(c) - grid(c)(j) - grid(3 * (i / 3) + c / 3)(3 * (j / 3) + c % 3)
    }

  def updated(i: Int, j: Int, value: Int): Sudoku = new Sudoku(grid.updated(i, grid(i).updated(j, value)))

  override def toString = "Sudoku:\n" + grid.map(_.mkString(" ") + "\n").mkString
}