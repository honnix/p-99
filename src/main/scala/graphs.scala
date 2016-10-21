package p99

case class Graph[T](nodes: List[T], edges: List[(T, T)])

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
  def spanningTree(graph: Graph[Char]) = {
    def select_edges(nodes: List[Char], edges: Stream[(Char, Char)]): Stream[List[(Char, Char)]] = {
      def accept(nodes: List[Char], edge: (Char, Char)) = {
        nodes.contains(edge._1) && !nodes.contains(edge._2) ||
        nodes.contains(edge._2) && !nodes.contains(edge._1)
      }

      if (nodes == Nil) Stream(Nil)
      else
        edges.filter(accept(nodes, _)).flatMap { x =>
          val nodes1 = nodes.filterNot(y => y == x._1 || y == x._2)
          val edges1 = edges.filterNot(_ == x)
          select_edges(nodes1, edges1).map(x :: _)
      }
    }
    select_edges(graph.nodes.tail, graph.edges.toStream).map(x => Graph(graph.nodes, x.sorted)).distinct
  }
}
