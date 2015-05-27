package rsi.graph

import scala.annotation.tailrec

object Prim {

  type Matrix = Int => Vector[Double]

  def mst(input: UndirectedGraph): UndirectedGraph = {
    val vertex = input.vertices.head
    val cheapestEdge = input.edges(vertex).minBy(_.weight)

    mst(input, UndirectedGraph(input.dim, cheapestEdge), input.vertices)
  }

  @tailrec
  private def mst(inputGraph: UndirectedGraph, currentPartial: UndirectedGraph, vertices: Set[Int]): UndirectedGraph = {
    if (currentPartial.vertices == vertices) currentPartial
    else {
      val verticesToSearch: Set[Int] = currentPartial.vertices
      val matrix: Matrix = inputGraph.matrix

      // get all edges connected to vertices of partial mst
      val reachable: Set[Edge] = findReachable(verticesToSearch.toSeq, matrix)
      // filter edges to vertices already in partial mst
      val candidates: Set[Edge] = notInCurrentMST(reachable, currentPartial)
      // add the cheapest to partial mst and recur
      val cheapestEdge: Edge = cheapest(candidates)

      mst(inputGraph, currentPartial + cheapestEdge, vertices)
    }
  }

  def findReachable(verticesToSearch: Seq[Int], matrix: Int => Vector[Double]): Set[Edge] = {
    verticesToSearch.flatMap(from => {
      matrix(from).zipWithIndex.collect {
        case (weight, to) if weight != Edge.UNREACHABLE => Edge(from, to, weight)
      }
    }).toSet
  }

  def notInCurrentMST(reachable: Set[Edge], partial: UndirectedGraph): Set[Edge] = {
    reachable.filterNot { case Edge(from, to, _) => partial.matrix(from)(to) != Edge.UNREACHABLE }
  }

  def cheapest(candidates: Set[Edge]): Edge = {
    candidates.minBy(_.weight)
  }
}
