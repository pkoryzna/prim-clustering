package rsi.graph

import scala.annotation.tailrec

object Prim {

  type Matrix = Int => Vector[Double]

  def mst(input: UndirectedGraph): UndirectedGraph = {
    val vertex = input.vertices.head
    val cheapestEdge = input.edges(vertex).minBy(_.weight)

    mst(input.matrix, input.vertices, UndirectedGraph(input.dim, cheapestEdge), input.vertices)
  }

  @tailrec
  private def mst(matrix: Matrix, fromVertices: Set[Int], currentPartial: UndirectedGraph, allVertices: Set[Int]): UndirectedGraph = {
    if (currentPartial.vertices == allVertices) currentPartial
    else {
      // get all edges connected to vertices of partial mst
      val reachable: Set[Edge] = findReachable(fromVertices.toSeq, matrix)
      // filter edges to vertices already in partial mst
      val candidates: Set[Edge] = notInCurrentMST(reachable, currentPartial)
      // add the cheapest to partial mst and recur
      val cheapestEdge: Edge = cheapest(candidates).get

      mst(matrix, fromVertices + cheapestEdge.to + cheapestEdge.from, currentPartial + cheapestEdge, allVertices)
    }
  }
  
  def cheapestInPartition(matrix: PartitionedMatrix, currentPartial: UndirectedGraph, allVertices: Set[Int]): Option[Edge] = {
    // get all edges connected to vertices of partial mst within current partition
    val reachable: Set[Edge] = findReachable(matrix.range.intersect(currentPartial.vertices.toSeq), matrix(_))
    // filter edges to vertices already in partial mst
    val candidates: Set[Edge] = notInCurrentMST(reachable, currentPartial)
    // return the optional cheapest candidate
    cheapest(candidates)
  }

  // add the cheapest edge from those obtained from partitions to partial mst graph
  def partialFromPartitions(cheapestEdges: List[Option[Edge]], currentPartial: UndirectedGraph) : UndirectedGraph = {
    currentPartial + cheapestEdges.flatten.minBy(_.weight)
  }

  def findReachable(fromVertices: Seq[Int], matrix: Int => Vector[Double]): Set[Edge] = {
    fromVertices.flatMap(from => {
      matrix(from).zipWithIndex.collect {
        case (weight, to) if weight != Edge.UNREACHABLE => Edge(from, to, weight)
      }
    }).toSet
  }

  def notInCurrentMST(reachable: Set[Edge], partial: UndirectedGraph): Set[Edge] = {
    reachable.filterNot { case Edge(from, to, _) => partial.matrix(from)(to) != Edge.UNREACHABLE }
  }

  def cheapest(candidates: Set[Edge]): Option[Edge] = {
    if(candidates.isEmpty) None
    else Some(candidates.minBy(_.weight))
  }
}
