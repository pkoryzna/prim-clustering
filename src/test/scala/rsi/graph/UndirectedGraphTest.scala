package rsi.graph

import org.specs2.ScalaCheck

import scala.util.Random

class UndirectedGraphTest extends org.specs2.mutable.Specification with ScalaCheck {
  "UndirectedGraph" should {

    val edges = Edge(1, 2, 999.0) :: Edge(2, 3, 100.0) :: Edge(3, 1, 69.0) :: Nil
    val graph = UndirectedGraph(10, edges)

    "construct an adjacency matrix from a list of edges" in {
      edges forall { edge =>
        graph.matrix(edge.from)(edge.to) shouldEqual graph.weight(edge.from, edge.to)
        graph.weight(edge.from, edge.to) shouldEqual edge.weight
      }
    }

    "have equal weights of edges both ways" in {
      edges forall { case Edge(from, to, weight) =>
        graph.matrix(from)(to) shouldEqual (graph.matrix(to)(from))
      }
    }

    "support adding new edges to the graph" in {
      val newEdge = Edge(5,6,999.0)

      (graph + newEdge).weight(newEdge.from, newEdge.to) must beEqualTo(newEdge.weight)

      (graph + newEdge).weight(newEdge.to, newEdge.from) must beEqualTo(newEdge.weight)

    }
  }

  "PartitionedMatrix" should {
    val edges = Edge(1, 2, 999.0) :: Edge(2, 3, 100.0) :: Edge(3, 1, 69.0) :: Nil
    val graph = UndirectedGraph(10, edges)


    "contain all of the indexes when merged" in {
      val partitions: Vector[PartitionedMatrix] = graph.partition(2)
      val mergedBack = PartitionedMatrix.merge(partitions)

      mergedBack must beEqualTo(graph.matrix)

    }

    "not explode for a graph of any dimension" in {
      (2 to 30) map { dim => // todo figure out the exception when using scalacheck

        val edges = 0 to dim map { _ => Edge(Random.nextInt(dim), Random.nextInt(dim), Random.nextDouble()) }
        val graph = UndirectedGraph(dim, edges.toList)

        val partitions: Vector[PartitionedMatrix] = graph.partition(Random.nextInt(dim-1)+1)
        val mergedBack = PartitionedMatrix.merge(partitions)

        mergedBack must beEqualTo(graph.matrix)
      }

    }
  }
}
