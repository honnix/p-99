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
      ('e', 'h'), ('d', 'e'), ('d', 'f'), ('d', 'g'), ('g', 'h'))
  )

  val graph3 = WGraph(
    List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
    List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4), ('c', 'e', 6),
      ('e', 'h', 5), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3), ('g', 'h', 1))
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
}
