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
      val l1 = gray(tail, flag ^ 1)
      head + flag.toString() :: head + (flag ^ 1).toString() :: l1
  }

  // 3.05
  def huffman(fs: List[(Char, Int)]) = {
    object FreqOrdering extends Ordering[(Char, Int, AnyRef, AnyRef)] {
      def compare(a: (Char, Int, AnyRef, AnyRef),
        b: (Char, Int, AnyRef, AnyRef)) = b._2 - a._2
    }

    import collection.mutable.PriorityQueue
    def buildTree(q: PriorityQueue[(Char, Int, AnyRef, AnyRef)]):
        (Char, Int, AnyRef, AnyRef) = {
      if (q.size == 1)
        q.dequeue()
      else {
        val e1 = q.dequeue()
        val e2 = q.dequeue()
        q.enqueue((0, e1._2 + e2._2, e1, e2))
        buildTree(q)
      }
    }

    def tranverse(t: (Char, Int, AnyRef, AnyRef), cur: String): List[(Char, String)] = {
      if (t._3 == null)
        List((t._1, cur))
      else {
        val list = tranverse(t._3.asInstanceOf[(Char, Int, AnyRef, AnyRef)], cur + "0") :::
        tranverse(t._4.asInstanceOf[(Char, Int, AnyRef, AnyRef)], cur + "1")
        if (t._1 != 0)
          (t._1, cur) :: list
        else list
      }
    }

    val q = collection.mutable.PriorityQueue(
      fs.map(x =>
        (x._1, x._2, null.asInstanceOf[AnyRef], null.asInstanceOf[AnyRef])
      ): _*)(FreqOrdering)
    tranverse(buildTree(q), "")
  }
}
