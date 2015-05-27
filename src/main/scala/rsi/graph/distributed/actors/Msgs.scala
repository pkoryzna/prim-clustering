package rsi.graph.distributed.actors

import akka.actor.ActorRef
import rsi.graph.{Edge, UndirectedGraph, PartitionedMatrix}

object Msgs {
  case class FindMst(graph: UndirectedGraph, partial: Option[UndirectedGraph] = None, solutionReceiver: Option[ActorRef] = None)
  case class FindCheapestInPartition(matrix: PartitionedMatrix, currentPartial: UndirectedGraph, allVertices: Set[Int])
  case class EdgeResult(edgeOpt: Option[Edge])
  case class JobFailed(reason: String, job: FindMst)
  case object BackendRegistration

}
