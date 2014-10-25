package euler
package til70

object Euler68 extends EulerProblem {

  override def result = {
    // each possible magic 5-gon ring
    val rings = for {
      inners <- (1 to 9).toList.combinations(5).flatMap(_.permutations)
      rest = (1 to 10).toSet -- inners
      outers <- rest.toList.permutations
    } yield inners ::: outers

    /*      i5
     *        i0   i6
     *    i4     i1
     *  i9  i3  i2 i7
     *       i8      
     */
    val res = rings map { // lines
      case i0 :: i1 :: i2 :: i3 :: i4 :: i5 :: i6 :: i7 :: i8 :: i9 :: Nil =>
        List(List(i5, i0, i1), List(i6, i1, i2), List(i7, i2, i3), List(i8, i3, i4), List(i9, i4, i0))
      case _ => Nil
    } filter { // must have same size
      case l1 :: l2 :: l3 :: l4 :: l5 :: Nil =>
        l1.sum == l2.sum && l2.sum == l3.sum && l3.sum == l4.sum && l4.sum == l5.sum
      case _ => false
    } map { // map to the clockwise rotation with the lowest external node
      rotations(_).minBy(_.head.head)
    } maxBy { // find maximum
      _.mkString
    }

    res.flatten.mkString
  }

  def rotations[T](xs: List[T]): List[List[T]] = {
    (0 until xs.size).map { i =>
      val (h, t) = xs.splitAt(i)
      t ::: h
    }.toList
  }
}

