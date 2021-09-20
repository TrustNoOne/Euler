package euler
package til90

import Utils._

object Euler81 extends EulerProblem {

  //the matrix is basically a DAG sorted topologically
  override def result = {
    val matrix = readMatrix()
    val matrixSize = matrix.size

    val costs = Array.fill[Int](matrixSize, matrixSize)(Integer.MAX_VALUE)
    costs(0)(0) = matrix(0)(0)

    for {
      x1 <- 0 until (matrixSize * 2 - 1)
      x2 = math.min(matrixSize - 1, x1)
      j <- x1 - x2 to x2
      i = x1 - j
      //(i,j) = node indexes in topological order
    } {
      //indexes of destinations
      val dests = Seq((i + 1, j), (i, j + 1)) filter {
        case (i, j) => i < matrixSize && j < matrixSize
      }

      //update costs
      dests foreach {
        case (id, jd) =>
          val newCost = costs(i)(j) + matrix(id)(jd)
          if (costs(id)(jd) > newCost) costs(id)(jd) = newCost
      }
    }

    costs(matrixSize - 1)(matrixSize - 1)
  }

  def readMatrix() = withResource("matrix.txt") { src =>
    src.getLines().map(_.split(',').map(_.toInt)).toVector
  }
}
