package rsi.graph

import scala.annotation.tailrec

object Prim {

  def mst(input: UndirectedGraph): UndirectedGraph = {
    val vertex = input.vertices.head
    val cheapestEdge = input.edges(vertex).minBy(_.weight)

    mst(input, UndirectedGraph(input.dim, cheapestEdge), input.vertices)
  }

  @tailrec
  private def mst(inputGraph: UndirectedGraph, partial: UndirectedGraph, vertices: Set[Int]): UndirectedGraph = {
    if (partial.vertices == vertices) partial
    else {
      // get all edges connected to vertices of partial mst
      val edgesLeft: Set[Edge] = partial.vertices.flatMap(inputGraph.edges)
      // filter edges to vertices already in partial mst
      val candidates: Set[Edge] = edgesLeft.filterNot { case Edge(from, to, _) => partial.matrix(from)(to) != Edge.UNREACHABLE }
      // add the cheapest to partial mst and recur
      val cheapest: Edge = candidates.minBy(_.weight)

      mst(inputGraph, partial + cheapest, vertices)
    }
  }
}
