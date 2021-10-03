package euler
package til90

import Utils._
import scala.collection.parallel.CollectionConverters._

object Euler82 extends EulerProblem {

  val matrix = withResource("matrix.txt") { src => src.getLines().map(_.split(',').map(_.toInt)).toVector }
  val matrixSize = matrix.size

  override def result() = {
    (0 until matrixSize).par.map(minCostStartingFrom(_, 0)).min
  }

  //dijkstra
  def minCostStartingFrom(i: Int, j: Int) = {
    val costs = Array.fill[Int](matrixSize, matrixSize)(Integer.MAX_VALUE)
    val visited = Array.fill[Boolean](matrixSize, matrixSize)(false)

    @annotation.tailrec
    def visit(elem: (Int, Int)): Unit = elem match {
      case (i, j) =>
        //find indexes of destinations
        val dests = Seq((i + 1, j), (i, j + 1), (i - 1, j)) filter { case (i, j) =>
          i < matrixSize && j < matrixSize && i >= 0
        }

        if (!dests.isEmpty) {
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
    }

    costs(i)(j) = matrix(i)(j)
    visit((i, j))
    // min of the last column = min path
    costs.map(_(matrixSize - 1)).min
  }

}
