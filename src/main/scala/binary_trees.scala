package p99

object BinaryTrees {
  case class Node[T](value: T, left: Node[T], right: Node[T])

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
}
