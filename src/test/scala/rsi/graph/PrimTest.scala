package rsi.graph

import org.specs2.mutable.Specification

class PrimTest extends Specification {
  "Prim implementation" should {
    "return a graph containing all of the vertices of original" in {
      "single edge" >> {
        val graph = UndirectedGraph(2, Edge(0, 1, 1.0))
        Prim.mst(graph).vertices should beEqualTo(graph.vertices)
      }

      "three edges (simple)" >> {
        val graph = UndirectedGraph(3, Edge(0, 1, 1.0), Edge(0, 2, 1.0), Edge(1, 2, 99999.0))

        Prim.mst(graph).vertices should beEqualTo(graph.vertices)
      }
    }

    "find a minimal spanning tree" in {

      "single edge" >> {
        val graph = UndirectedGraph(2, Edge(0, 1, 1.0))
        Prim.mst(graph) must beEqualTo(graph)
      }

      "three edges (simple)" >> {
        val graph = UndirectedGraph(3, Edge(0, 1, 1.0), Edge(0, 2, 1.0), Edge(1, 2, 99999.0))

        Prim.mst(graph) must beEqualTo(UndirectedGraph(3, Edge(0, 1, 1.0), Edge(0, 2, 1.0)))
      }

      "harder graph" >> {
        val graph = UndirectedGraph(5, Edge(0, 1, 1.0),
          Edge(0, 2, 9999.0), Edge(0, 3, 8888.0),
          Edge(1, 4, 1.0), Edge(2, 4, 1.0), Edge(3, 4, 1.0)
        )

        Prim.mst(graph) must beEqualTo(UndirectedGraph(5, Edge(0, 1, 1.0), Edge(1, 4, 1.0), Edge(2, 4, 1.0), Edge(3, 4, 1.0)
        ))
      }

    }
  }
}
