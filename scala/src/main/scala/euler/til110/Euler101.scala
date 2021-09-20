package euler
package til110

object Euler101 extends EulerProblem {

  override def result = {
    // val un = Vector(1, 683, 44287, 838861, 8138021, 51828151, 247165843, 954437177, 3138105961L, 9090909091L, 23775972551L)
    //  un = 1 − n + n2 − n3 + n4 − n5 + n6 − n7 + n8 − n9 + n10
    (1 to 10).map(x => genseq(op(x): _*)(x + 1)).sum
  }

  def op(k: Int) = {
    val seq = (1 to k) map genseq(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1)

    val matrix = (1 to k).zip(seq).map { case (x, u) =>
      (0 to k - 1).map(e => math.pow(x.toDouble, e.toDouble).toLong) :+ u
    }

    new System(matrix).solved
  }

  // generates the nth value in the sequence modeled by the polynomial with given coefficients
  def genseq(coefficients: Long*)(n: Int) =
    coefficients.zipWithIndex.foldLeft(0L) { case (acc, (x, e)) => acc + x * math.pow(n.toDouble, e.toDouble).toLong }

  case class System(grid: Seq[Seq[Long]]) {
    def solved = {
      def firstPass(grid: Seq[Seq[Long]], r: Int): Seq[Seq[Long]] = r match {
        case x if x == grid.size =>
          (0 until grid.size).foldLeft(grid)((tmpGrid, i) => tmpGrid.updated(i, divideByElem(grid(i), i)))
        case _ =>
          val newGrid = (r until grid.size).foldLeft(grid)((tmpGrid, i) =>
            tmpGrid.updated(i, subtractRow(grid(i), grid(r - 1), r - 1)),
          )
          firstPass(newGrid, r + 1)
      }

      def secondPass(grid: Seq[Seq[Long]], r: Int): Seq[Seq[Long]] = r match {
        case 0 =>
          (0 until grid.size).foldLeft(grid)((tmpGrid, i) => tmpGrid.updated(i, divideByElem(grid(i), i)))
        case _ =>
          val newGrid = (0 until r).foldLeft(grid)((tmpGrid, i) => tmpGrid.updated(i, subtractRow(grid(i), grid(r), r)))
          secondPass(newGrid, r - 1)
      }

      val solution = secondPass(firstPass(grid, 1), grid.size - 1)
      solution.map(_.last)
    }

    private def subtractRow(r1: Seq[Long], r2: Seq[Long], pivotIdx: Int) = {
      val r1Elem = r1(pivotIdx)
      val r2Elem = r2(pivotIdx)
      val rlcm = lcm(r1Elem, r2Elem)

      r1.map(_ * rlcm / r1Elem)
        .zip(r2.map(_ * rlcm / r2Elem))
        .map(x => x._1 - x._2)
    }

    private def divideByElem(row: Seq[Long], idx: Int) = {
      val elem = row(idx)
      row.map(_ / elem)
    }

  }

}
