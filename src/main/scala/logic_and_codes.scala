package p99

object LogicAndCodes {
  // 3.01
  def table(f: (Boolean, Boolean) => Boolean) {
    for (x <- List(true, false); y <- List(true, false))
      println(s"$x $y ${f(x, y)}")
  }
}
