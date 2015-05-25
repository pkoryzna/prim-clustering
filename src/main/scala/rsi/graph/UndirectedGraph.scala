package rsi.graph

import scala.language.implicitConversions


case class Edge(from: Int, to: Int, weight: Double)

object Edge {
  val UNREACHABLE = 0.0d
  implicit def tuple2edge(ftw: (Int, Int, Double)): Edge = Edge.apply _ tupled ftw
}

case class PartitionedMatrix(vector: Vector[Vector[Double]], offset: Int) {
  def apply(n: Int): Vector[Double] = vector(n+offset)
}

object PartitionedMatrix{
  def merge(partitions: Vector[PartitionedMatrix]): Vector[Vector[Double]] = {
    partitions.foldLeft(Vector[Vector[Double]]()) { case (res, partition) =>
      res ++ partition.vector
    }
  }
}


case class UndirectedGraph(dim: Int, matrix: Vector[Vector[Double]]) {
  def reachableFrom(vertex: Int): Seq[(Double, Int)] = {
    matrix(vertex).zipWithIndex.collect {
      case (weight, idx) if weight != Edge.UNREACHABLE => (weight, idx)
    }
  }

  def weight(from: Int, to: Int): Double = matrix(from)(to)


  /**
   * Split the adjacency matrix.
   * @param partitionCount count of pieces to be distributed
   * @return vector of split parts
   */
  def partition(partitionCount: Int): Vector[PartitionedMatrix] = {
    require(partitionCount != 0)
    val partitionSize: Int = Math.ceil(matrix.size.toDouble / partitionCount.toDouble).toInt

    Vector.tabulate(partitionCount){n => PartitionedMatrix(matrix.slice(n*partitionSize, (n+1)*partitionSize), n*partitionSize)}
  }
}


object UndirectedGraph {
  def apply(dim: Int, edges: List[Edge]) = {
    val arr = Array.fill[Double](dim, dim)(Edge.UNREACHABLE)

    edges.foreach { e =>
      arr(e.from)(e.to) = e.weight; arr(e.to)(e.from) = e.weight
    }

    val matrixVec = arr.map(_.toVector).toVector // this needs to be immutable

    new UndirectedGraph(dim, matrixVec)
  }


  def apply(dim: Int, edges: Edge*): UndirectedGraph = {
    apply(dim, edges.toList)
  }

}


