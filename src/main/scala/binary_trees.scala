package p99

case class Node[T](value: T, left: Node[T], right: Node[T]) {
  def this(value: T) { this(value, null, null) }
}

object BinaryTrees {

  // 4.01 doesn't make sense for scala

  // 4.02
  def cbalTree(nodes: Int): Stream[Node[Char]] = {
    import language.postfixOps
    if (nodes == 0) Stream(null)
    else {
      val n1 = (nodes - 1) / 2
      val n2 = nodes - 1 - n1
      if (n1 == n2)
        cbalTree(n1).map {x =>
          cbalTree(n2).map {y =>
            Node('x', x, y)
          }
        } flatten
        else {
          cbalTree(n1).map {x =>
            cbalTree(n2).map {y =>
              Node('x', x, y)
            }
          } ++ cbalTree(n2).map {x =>
            cbalTree(n1).map {y =>
              Node('x', x, y)
            }
          } flatten
        }
    }
  }

  // 4.03
  def symmetric[T](tree: Node[T]) = {
    def mirror[S](tree1: Node[S], tree2: Node[S]): Boolean = {
      tree1 == null && tree2 == null ||
      tree1 != null && tree2 != null &&
      mirror(tree1.left, tree2.right) && mirror(tree1.right, tree2.left)
    }

    tree == null || mirror(tree.left, tree.right)
  }

  // 4.04
  def construct[T: Ordering](list: List[T]) = {
    def construct0[S: Ordering](list: List[S], cur: Node[S]): Node[S] = list match {
      case Nil => cur
      case head :: tail =>
        construct0(tail, add(head, cur))
    }

    import Ordering.Implicits._
    def add[S: Ordering](e: S, cur: Node[S]): Node[S] = {
      if (cur == null) Node(e, null, null)
      else if (e <= cur.value) {
        Node(cur.value, add(e, cur.left), cur.right)
      }
      else Node(cur.value, cur.left, add(e, cur.right))
    }

    construct0(list, null)
  }

  def testSymmetric[T: Ordering](list: List[T]) =
    symmetric(construct(list))
}
