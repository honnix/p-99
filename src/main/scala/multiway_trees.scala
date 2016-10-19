package p99

case class MNode[T](value: T, children: List[MNode[T]])

object MultiwayTrees {
  // 5.01 doesn't make sense for scala

  // 5.02
  def nnodes[T](tree: MNode[T]): Int = {
    1 + tree.children.foldLeft(0)(_ + nnodes(_))
  }

  // 5.03
  def tree(s: String): MNode[Char] = {
    def tree0(l: List[Char]): (List[Char], MNode[Char]) = {
      val (l1, children) = trees(l.tail)
      (l1, MNode(l.head, children))
    }

    def trees(l: List[Char]): (List[Char], List[MNode[Char]]) = l match {
      case '^' :: tail => (tail, Nil)
      case _ =>
        val (l1, head) = tree0(l)
        val (l2, tail) = trees(l1)
        (l2, head :: tail)
    }

    tree0(s.toList)._2
  }
}
