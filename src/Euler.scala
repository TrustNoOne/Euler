object Euler {
  def elapsed(t: => Any) = {
    val t0 = System.currentTimeMillis()
    t.toString + " in " + (System.currentTimeMillis() - t0) + " ms"
  }
}