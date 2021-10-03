package euler
package til90

import Utils._

object Euler83 extends EulerProblem {

  val matrix = withResource("matrix.txt") { src => src.getLines().map(_.split(',').map(_.toInt)).toVector }
  val matrixSize = matrix.size

  //dijkstra
  override def result() = {
    val costs = Array.fill[Int](matrixSize, matrixSize)(Integer.MAX_VALUE)
    val visited = Array.fill[Boolean](matrixSize, matrixSize)(false)

    @annotation.tailrec
    def visit(elem: (Int, Int)): Unit = elem match {
      case (i, j) =>
        //find indexes of destinations
        val dests = Seq((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)) filter { case (i, j) =>
          i < matrixSize && j < matrixSize && i >= 0 && j >= 0
        }

        //update costs for destinations
        dests foreach { case (id, jd) =>
          val newCost = costs(i)(j) + matrix(id)(jd)
          if (costs(id)(jd) > newCost) costs(id)(jd) = newCost
        }
        visited(i)(j) = true

        // Select the unvisited node that is marked with the smallest tentative distance/cost
        val unvisitedIt = for {
          i <- Iterator.from(0).take(matrixSize)
          j <- Iterator.from(0).take(matrixSize)
          if !visited(i)(j)
        } yield (i, j)

        if (unvisitedIt.hasNext) {
          val next = unvisitedIt minBy { case (i, j) => costs(i)(j) }
          visit(next)
        }
    }

    costs(0)(0) = matrix(0)(0)
    visit((0, 0))

    costs(matrixSize - 1)(matrixSize - 1)
  }

}
