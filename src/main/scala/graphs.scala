package p99

case class Graph[T](nodes: List[T], edges: List[(T, T)])
case class WGraph[T](nodes: List[T], edges: List[(T, T, Int)])

object Graphs {
  val graph1 = Graph(
    List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
    List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h'))
  )

  val graph2 = Graph(
    List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
    List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'), ('c', 'e'),
      ('e', 'h'), ('d', 'e'), ('d', 'f'), ('d', 'g'), ('f', 'g'), ('g', 'h'))
  )

  val graph3 = WGraph(
    List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
    List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4), ('c', 'e', 6),
      ('e', 'h', 5), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3), ('f', 'g', 4),
      ('g', 'h', 1))
  )

  def neighbours[T](graph: Graph[T], node: T) = {
    if (!graph.nodes.contains(node)) Nil
    else
      graph.edges.collect {
        case (x, neighbour) if (x == node) => neighbour
        case (neighbour, x) if (x == node) => neighbour
      }
  }

  // 6.02
  def path[T](graph: Graph[T], from: T, to: T) = {
    def path0[S](graph: Graph[S], from: S, to: S, visited: List[S]): Stream[List[S]] = {
      if (from == to) Stream(List(to))
      else
        neighbours(graph, from).toStream.filterNot(visited.contains(_)).flatMap { x =>
          path0(graph, x, to, from :: visited).map(from :: _)
      }
    }

    path0(graph, from, to, Nil)
  }

  // 6.03
  def cycle[T](graph: Graph[T], from: T) =
    neighbours(graph, from).toStream.flatMap(path(graph, from, _)).filter(_.length > 2)
      .map(_ ::: List(from))

  // 6.04
  def select_edges[T](nodes: List[T], edges: Stream[(T, T, Int)]): Stream[List[(T, T, Int)]] = {
    def accept(nodes: List[T], edge: (T, T, Int)) = {
      nodes.contains(edge._1) && !nodes.contains(edge._2) ||
      nodes.contains(edge._2) && !nodes.contains(edge._1)
    }

    println(edges.toList)
    if (nodes == Nil) Stream(Nil)
    else
      edges.filter(accept(nodes, _)).flatMap { x =>
        val nodes1 = nodes.filterNot(y => y == x._1 || y == x._2)
        val edges1 = edges.filterNot(_ == x)
        select_edges(nodes1, edges1).map(x :: _)
      }
  }

  def spanningTree[T](graph: WGraph[T]) = {
    select_edges(graph.nodes.tail, graph.edges.toStream).map { x =>
      WGraph(graph.nodes, x.sortWith { (a, b) =>
        val s1 = a._1.toString
        val s2 = a._2.toString
        val s3 = b._1.toString
        val s4 = b._2.toString
        s1 < s3 || s1 == s3 && s2 < s4
      })
    }.distinct    
  }

  // 6.05
  def minimalSpanningTree[T](graph: WGraph[T]) = {
    select_edges(graph.nodes.tail, graph.edges.sortBy(_._3).toStream).map { x =>
      WGraph(graph.nodes, x.sortWith { (a, b) =>
        val s1 = a._1.toString
        val s2 = a._2.toString
        val s3 = b._1.toString
        val s4 = b._2.toString
        s1 < s3 || s1 == s3 && s2 < s4
      })
    }.head.edges.foldLeft(0)(_ + _._3)
  }

  // 6.07
  def degree[T](graph: Graph[T], node: T) = {
    if (!graph.nodes.contains(node)) -1
    else
      graph.edges.foldLeft(0) { (x, y) =>
        y match {
          case (n, _) if (n == node) => x + 1
          case (_, n) if (n == node) => x + 1
          case _ => x
        }
      }
  }

  def degList[T](graph: Graph[T]) =
    graph.nodes.sortWith(degree(graph, _) >= degree(graph, _))

  def isNeighbour[T](edges: List[(T, T)], node1: T, node2: T) =
    edges.exists { x =>
      x. _1 == node1 && x._2 == node2 ||
      x. _2 == node1 && x._1 == node2
    }

  def coloring[T](graph: Graph[T]) = {
    def coloring[S](edges: List[(S, S)], nodes: List[S], c: Int): List[(S, Int)] = nodes match {
      case Nil => Nil
      case head :: tail =>
        val (colored, l1) = coloring0(edges, tail, List((head, c)), List(head), c)
        l1 ::: coloring(edges, tail.filterNot(colored.contains(_)), c + 1)
    }

    def coloring0[S](edges: List[(S, S)], nodes: List[S], l: List[(S, Int)],
      colored: List[S], c: Int): (List[S], List[(S, Int)]) = nodes match {
      case Nil => (colored, l)
      case head :: tail =>
        if (!colored.exists(isNeighbour(edges, _, head)))
          coloring0(edges, tail, (head, c) :: l, head :: colored, c)
        else
          coloring0(edges, tail, l, colored, c)
    }

    val nodes = degList(graph)
    coloring(graph.edges, nodes, 1)
  }

  // 6.08
  def tranverse[T](graph: Graph[T], node: T) = {
    def tranverse0[S](nodes: List[S], graph: Graph[S],
      visited: List[S]): List[S] = nodes match {
      case Nil => visited
      case head :: tail =>
        val visited1 = if (!visited.contains(head)) {
          val n = neighbours(graph, head)
          tranverse0(n, graph, head :: visited)
        } else visited
        tranverse0(tail, graph, visited1)
    }

    tranverse0(List(node), graph, Nil).reverse
  }

  // 6.09
  def split[T](graph: Graph[T]) = {
    def split0[S](nodes: List[S], graph: Graph[S]): List[List[S]] = nodes match {
      case Nil => Nil
      case head :: tail =>
        val component = tranverse(graph, head)
        component :: split0(nodes.filterNot(component.contains(_)), graph)
    }

    split0(graph.nodes, graph)
  }

  // 6.10
  def isBipartite[T](graph: Graph[T]) = {
    def neighbours[S](edges: List[(S, S)], node: S) =
      edges.collect {
        case (x, neighbour) if (x == node) => neighbour
        case (neighbour, x) if (x == node) => neighbour
      }

    def isBipartite0[S](nodes: List[S], edges: List[(S, S)],
      left: List[S], right: List[S]): (List[S], List[S], List[(S, S)], Boolean) = edges match {
      case Nil => (left, right, edges, true)
      case _ =>
        val n = neighbours(edges, nodes.head)
        if (left.exists(n.contains(_))) (left, right, edges, false)
        else {
          val newEdges = edges.filterNot(x => x._1 == nodes.head || x._2 == nodes.head)
          val (left1, right1, newEdges1, result) = isBipartite0(n, newEdges, n ::: right, left)
          if (!result)
            (left1, right1, newEdges1, result)
          else
            isBipartite0(nodes.tail, newEdges1, left1, right1)
        }
    }

    // assume graph is connected
    isBipartite0(List(graph.nodes.head), graph.edges, List(graph.nodes.head), Nil)
  }
}
