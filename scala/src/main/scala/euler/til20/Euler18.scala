package euler
package til20

import Utils._

object Euler18 extends EulerProblem { //Ok for Euler67 too
  override def result = solve("triangle18.txt")

  def solve(triangleFile: String) = withResource(triangleFile) { src =>
    val tree = src.getLines().map { _.split(" ").map(_.toInt) }.toIndexedSeq
    src.close

    val numLines = tree.size
    val lengthTo = for (i <- 1 to numLines) yield Array.ofDim[Int](i)

    tree.zipWithIndex foreach {
      case (row, i) if i < numLines - 1 =>
        row.zipWithIndex foreach {
          case (_, j) =>
            lengthTo(i + 1)(j) =
              math.max(lengthTo(i + 1)(j), lengthTo(i)(j) + tree(i + 1)(j))
            lengthTo(i + 1)(j + 1) =
              math.max(lengthTo(i + 1)(j + 1),
                       lengthTo(i)(j) + tree(i + 1)(j + 1))
        }
      /* last line ignored*/
      case _ =>
    }

    lengthTo.flatten.max + tree(0)(0)
  }
}
