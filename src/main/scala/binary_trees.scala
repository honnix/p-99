package p99

import language.postfixOps

case class Node[T](value: T, left: Node[T], right: Node[T]) {
  def this(value: T) { this(value, null, null) }
}

object BinaryTrees {

  // 4.01 doesn't make sense for scala

  // 4.02
  def cbalTree(nodes: Int): Stream[Node[Char]] = {
    if (nodes == 0) Stream(null)
    else {
      val n1 = (nodes - 1) / 2
      val n2 = nodes - 1 - n1
      if (n1 == n2)
        cbalTree(n1).map { x =>
          cbalTree(n2).map { y =>
            Node('x', x, y)
          }
        } flatten
        else {
          cbalTree(n1).map { x =>
            cbalTree(n2).map { y =>
              Node('x', x, y)
            }
          } ++ cbalTree(n2).map { x =>
            cbalTree(n1).map { y =>
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

  // 4.05
  def symCbalTrees(n: Int) = {
    cbalTree(n) filter symmetric
  }

  def countSymCbalTrees(n: Int) =
    if (n % 2 == 0) 0
    else symCbalTrees(n).length

  // 4.06
  def hbalTree(h: Int): Stream[Node[Char]] = {
    if (h == 0) Stream(null)
    else if (h == 1) Stream(Node('x', null, null))
    else
      hbalTree(h - 1).map { x => 
        hbalTree(h - 1).map { y =>
          Node('x', x, y)
        }
      } ++ hbalTree(h - 2).map { x => 
        hbalTree(h - 1).map { y =>
          Node('x', x, y)
        }
      } ++ hbalTree(h - 1).map { x => 
        hbalTree(h - 2).map { y =>
          Node('x', x, y)
        }
      } flatten
  }

  // 4.07
  def hbalTreeNodes(n: Int) = {
    def minNodes(h: Int): Int =
      if (h == 0) 0
      else if (h == 1) 1
      else 1 + minNodes(h - 1) + minNodes(h - 2)

    def minHeight(n: Int): Int = {
      if (n == 0) 0
      else {
        val n1 = n / 2
        minHeight(n1) + 1
      }
    }

    def maxHeight(n: Int) = {
      def maxHeight0(n: Int, n1: Int, h: Int): Int = {
        if (n1 > n) h - 1
        else {
          val h1 = h + 1
          val n2 = minNodes(h1)
          maxHeight0(n, n2, h1)
        }
      }

      maxHeight0(n, 1, 1)
    }

    def nodes[T](r: Node[T]): Int = r match {
      case null => 0
      case Node(_, l, r) =>
        nodes(l) + nodes(r) + 1
    }

    println(minHeight(n))
    println(maxHeight(n))

    (minHeight(n) to maxHeight(n)).toStream.map { x =>
      hbalTree(x).filter(nodes(_) == n)
    } flatten
  }
}
