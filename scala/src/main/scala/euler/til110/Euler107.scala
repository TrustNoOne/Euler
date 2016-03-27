package euler
package til110

import euler.Utils.withResource

import scala.collection.mutable


/**
  * Minimum spanning tree
  */
object Euler107 extends EulerProblem {

  /*
   * Associate with each vertex v of the graph a number C[v] (the cheapest cost of a connection to v) and an edge E[v]
   * (the edge providing that cheapest connection). To initialize these values, set all values of C[v] to +∞
   * (or to any number larger than the maximum edge weight) and set each E[v] to a special flag value indicating that
   * there is no edge connecting v to earlier vertices.
   * Initialize an empty forest F and a set Q of vertices that have not yet been included in F (initially, all vertices).
   *
   * Repeat the following steps until Q is empty:
   * - Find and remove a vertex v from Q having the minimum possible value of C[v]
   * - Add v to F and, if E[v] is not the special flag value, also add E[v] to F
   * - Loop over the edges vw connecting v to other vertices w.
   *   For each such edge, if w still belongs to Q and vw has smaller weight than C[w], perform the following steps:
   *   - Set C[w] to the cost of edge vw
   *   - Set E[w] to point to edge vw.
   */

  case class Edge(from: Int, to: Int, weight: Int)

  case class Vertex(idx: Int, C: Int, E: Option[Edge] = None, conns: List[Edge])

  implicit object VertexOrdering extends Ordering[Vertex] {
    override def compare(x: Vertex, y: Vertex): Int = {
      val costDiff = x.C - y.C
      if (costDiff == 0) x.idx - y.idx else costDiff
    }
  }

  override def result = {
    val graph = parseGraph()
    val Qmap = mutable.Map(graph.map(v => v.idx -> v): _*)
    val Q = mutable.TreeSet(graph: _*)
    val graphCost = graph.flatMap(_.conns).map(_.weight).sum / 2

    def minSpanningTreeCost(result: Int = 0): Int = if (Q.isEmpty) result
    else {
      val v = Q.head
      Q.remove(v)
      Qmap.remove(v.idx)

      v.conns foreach { case edge@Edge(_, to, cost) =>
        //println(edge)
        Qmap.get(to).filter(cost < _.C) foreach { w =>
          Q -= w
          val newW = w.copy(C = cost, E = Some(edge))
          Q += newW
          Qmap += w.idx -> newW
        }
      }

      v.E match {
        case None => minSpanningTreeCost(result)
        case Some(edge) => minSpanningTreeCost(edge.weight + result)
      }
    }

    val saving = graphCost - minSpanningTreeCost()

    saving
  }


  def parseGraph() = withResource("p107_network.txt") { src =>
    src.getLines().map(_ split ",").zipWithIndex.map { case (line, idx) =>
      val conns = line.zipWithIndex
        .filter { case (weight, _) => weight != "-" }
        .map { case (weight, to) => Edge(idx, to, weight.toInt) }

      Vertex(idx, C = Int.MaxValue, E = None, conns.toList)
    }.toList
  }
}
