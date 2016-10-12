package p99

import language.postfixOps

case class Node[T](value: T, left: Node[T], right: Node[T])

case class PNode[T](value: T, left: PNode[T], right: PNode[T], x: Int, y: Int)

case class DNode[T](value: T)

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

  // 4.08
  def countLeaves[T](r: Node[T]): Int = r match {
    case null => 0
    case Node(_, null, null) => 1
    case Node(_, l, r) => countLeaves(l) + countLeaves(r)
  }

  // 4.09
  def leaves[T](r: Node[T]): List[DNode[T]] = r match {
    case null => Nil
    case Node(v, null, null) => List(DNode(v))
    case Node(_, l, r) => leaves(l) ++ leaves(r)
  }

  // 4.10
  def internals[T](r: Node[T]): List[DNode[T]] = r match {
    case null => Nil
    case Node(_, null, null) => Nil
    case Node(v, l, r) => DNode(v) :: internals(l) ++ internals(r)
  }

  // 4.11
  def atLevel[T](r: Node[T], l: Int) = {
    def atLevel[S](r: Node[S], l: Int, cur: Int): List[DNode[S]] =
      if (cur == l)
        List(DNode(r.value))
      else
        List(Option(r.left), Option(r.right)).flatMap { x =>
          x.map(atLevel(_, l, cur + 1)).getOrElse(Nil)
        }

    if (r == null) Nil else atLevel(r, l, 1)
  }

  def levelOrder[T](r: Node[T]) = {
    def levelOrder0[S](r: Node[S], cur: Int): List[DNode[S]] = {
      val l = atLevel(r, cur)
      if (l != Nil)
        l ++ levelOrder0(r, cur + 1)
      else l
    }

    levelOrder0(r, 1)
  }

  // 4.12
  def completeBinaryTree(n: Int): Node[Char] =
    if (n == 0)
      null
    else {
      val l = ((n - 1).toDouble / 2 ceil).toInt
      val r = n - 1 - l
      Node('x', completeBinaryTree(l), completeBinaryTree(r))
    }

  // 4.13
  def layoutBinaryTree[T](r: Node[T]) = {
    def layoutBinaryTree0[S](r: Node[S], level: Int, order: Int): (Int, PNode[S]) = r match {
      case null => (order, null)
      case Node(v, l, r) =>
        val (order1, pl) = layoutBinaryTree0(l, level + 1, order)
        val (order2, pr) = layoutBinaryTree0(r, level + 1, order1 + 1)
        (order2, PNode(v, pl, pr, order1 + 1, level))
    }

    layoutBinaryTree0(r, 1, 0)._2
  }

  // 4.14
  def layoutBinaryTree1[T](r: Node[T]) = {
    def distance[S](r: Node[S]): Int = r match {
      case null => 1
      case Node(v, l, r) =>
        2 * Math.max(distance(l), distance(r))
    }

    def layoutBinaryTree0[S](r: Node[S], level: Int, distance: Int): (Int, PNode[S]) = r match {
      case null => (0, null)
      case Node(v, l, r) =>
        val (x, pl) = layoutBinaryTree0(l, level + 1, distance / 2)
        val pr = layoutBinaryTree00(r, level + 1, x + distance, distance / 2)
        (x + distance / 2, PNode(v, pl, pr, x + distance / 2, level))
    }

    def layoutBinaryTree00[S](r: Node[S], level: Int, x: Int, distance: Int): PNode[S] = r match {
      case null => null
      case Node(v, l, r) =>
        val pl = layoutBinaryTree00(l, level + 1, x - distance / 2, distance / 2)
        val pr = layoutBinaryTree00(r, level + 1, x + distance / 2, distance / 2)
        PNode(v, pl, pr, x, level)
    }

    layoutBinaryTree0(r, 1, distance(r) / 2)._2
  }

  // 4.16
  def treeString[T](r: Node[T]): String = r match {
    case null => ""
    case Node(v, l, r) if (l == null && r == null) => v.toString
    case Node(v, l, r) => s"${v.toString}(${treeString(l)},${treeString(r)})"
  }

  def stringTree(s: String) = {
    def stringTree0(l: List[Char]): (List[Char], Node[Char]) = l match {
      case Nil => (Nil, null)
      case head :: '(' :: ',' :: tail =>
        val (rest, right) = stringTree0(tail)
        (rest, Node(head, null, right))
      case head :: '(' :: tail =>
        val (rest, left) = stringTree0(tail)
        val (rest1, right) = stringTree0(rest)
        (rest1, Node(head, left, right))
      case head :: ',' :: tail if (head != '(' && head != ')') =>
        (tail, Node(head, null, null))
      case head :: ')' :: tail if (head != ')') =>
        (tail, Node(head, null, null))
      case ',' :: next :: tail if (next == ')') =>
        (tail, null)
      case ',' :: next :: tail if (next != ')') =>
        stringTree0(next :: tail)
      case ')' :: tail => (tail, null)
    }

    stringTree0(s.toList)._2
  }

  // 4.17
  def preorder[T](r: Node[T]): String = r match {
    case null => ""
    case Node(v, l, r) => s"${v.toString}${preorder(l)}${preorder(r)}"
  }

  def inorder[T](r: Node[T]): String = r match {
    case null => ""
    case Node(v, l, r) => s"${preorder(l)}${v.toString}${preorder(r)}"
  }

  def preinTree(pre: String, in: String) = {
    def preinTree0(pre: List[Char], in: List[Char]): Node[Char] = {
      if (pre.isEmpty && in.isEmpty) null
      else {
        val i = in.indexOf(pre.head)
        val (il, ir0) = in.splitAt(i)
        val (pl, pr) = pre.tail.splitAt(il.length)
        Node(pre.head, preinTree0(pl, il), preinTree0(pr, ir0.tail))
      }
    }

    preinTree0(pre.toList, in.toList)
  }
}
