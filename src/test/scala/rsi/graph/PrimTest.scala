package rsi.graph

import org.specs2.mutable.Specification

class PrimTest extends Specification {
  "Prim implementation" should {
    "find a minimal spanning tree" in {
      "empty graph" >> {
        val graph = UndirectedGraph(0, Nil)
        Prim.mst(graph) must beEqualTo(graph)
      }

      "single edge" >> {
        val graph = UndirectedGraph(2, Edge(0, 1, 1.0))
        Prim.mst(graph) must beEqualTo(graph)
      }

      "three edges (simple)" >> {
        val graph = UndirectedGraph(3, Edge(0, 1, 1.0), Edge(0, 2, 1.0), Edge(1, 2, 99999.0))

        Prim.mst(graph) must beEqualTo(UndirectedGraph(3, Edge(0, 1, 1.0), Edge(0, 2, 1.0)))
      }


    }
  }
}
