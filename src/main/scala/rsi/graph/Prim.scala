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
      val edgesLeft = partial.vertices.flatMap(inputGraph.edges)
      // filter edges to vertices already in partial mst
      val candidates = edgesLeft.filterNot { case Edge(from, to, _) => partial.weight(from, to) != Edge.UNREACHABLE }
      // add the cheapest to partial mst and recur
      val cheapest = candidates.minBy(_.weight)

      mst(inputGraph, partial + cheapest, vertices)
    }
  }
}
