package p99

object LogicAndCodes {
  // 3.01
  def table(f: (Boolean, Boolean) => Boolean): Unit = {
    for (x <- List(true, false); y <- List(true, false))
      println(s"$x $y ${f(x, y)}")
  }

  // 3.04
  def gray(n: Int): List[String] = n match {
    case 1 => List("0", "1")
    case _ =>
      gray(gray(n - 1), 0)
  }

  def gray(l: List[String], flag: Int): List[String] = l match {
    case Nil => Nil
    case head :: tail =>
      val l1 = gray(tail, (flag + 1) % 2)
      head + flag.toString() :: head + ((flag + 1) % 2).toString() :: l1
  }
}
