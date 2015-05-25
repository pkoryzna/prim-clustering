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

  /**
   * @param from from which we are going out 
   * @return vertices that can be reached from given vertex with their respective weights 
   */
  def reachableFrom(from: Int): Seq[(Int, Double)] = {
    matrix(from).zipWithIndex.collect {
      case (weight, vertex) if weight != Edge.UNREACHABLE => (vertex, weight)
    }
  }

  /**
   * @return vertices which have at least one edge connecting them
   */
  def vertices: Set[Int] = matrix.zipWithIndex.filterNot(_._1.forall(_ == Edge.UNREACHABLE)).collect {case (_, idx) => idx}.toSet

  /**
   * @return weight of an edge connecting vertices a and b
   */
  def weight(a: Int, b: Int): Double = matrix(a)(b)

  /**
   * @return adds edge to graph returning a new copy
   */
  def +(edge: Edge): UndirectedGraph = edge match {
    case Edge(from, to, weight) => {
      val upd: Vector[Vector[Double]] = matrix.updated(from, matrix(from).updated(to, weight))
      val `'upd`: Vector[Vector[Double]] = upd.updated(to, matrix(to).updated(from, weight))

      this.copy(matrix = `'upd`)
    }
  }

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


