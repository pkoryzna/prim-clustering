package rsi.graph.distributed.actors

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated, _}
import akka.event.LoggingReceive
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import rsi.graph.distributed.actors.Msgs._
import rsi.graph.{Edge, Prim, UndirectedGraph}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

//#frontend
class PrimFrontend extends Actor {
  implicit val timeout = Timeout(5 seconds)
  implicit val futureCtx = scala.concurrent.ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  var backends = IndexedSeq.empty[ActorRef]

  def receive = LoggingReceive {
    case job: FindMst if backends.isEmpty =>
      sender() ! JobFailed("Service unavailable, try again later", job)

    case BackendRegistration if !backends.contains(sender()) =>
      context watch sender()
      backends = backends :+ sender()

    case Terminated(a) =>
      backends = backends.filterNot(_ == a)

    case FindMst(graph: UndirectedGraph, None, _) =>
      val vertex = graph.vertices.head
      val cheapestEdge = graph.edges(vertex).minBy(_.weight)
      self ! FindMst(graph, Some(UndirectedGraph(graph.dim, cheapestEdge)), Some(sender()))

    case FindMst(graph: UndirectedGraph, Some(partial), rcvRef) if partial.vertices != graph.vertices =>
      val partitioned = graph.partition(backends.size)
      val responses: IndexedSeq[Future[Any]] = backends.zipWithIndex.map { case (worker, i) =>
        worker ? FindCheapestInPartition(partitioned(i), partial, graph.vertices)
      }
      val res = Future.sequence(responses)
      res.onSuccess { case results: IndexedSeq[EdgeResult] =>
        val foundEdges = results.collect { case EdgeResult(edgeOpt) => edgeOpt }
        val newPartial = Prim.partialFromPartitions(foundEdges.toList, partial)

        self ! FindMst(graph, Some(newPartial), rcvRef)
      }

    case FindMst(graph, Some(solution), Some(origSender)) if solution.vertices == graph.vertices =>
      origSender ! solution


  }
}

//#frontend

object PrimFrontend {
  def main(args: Array[String]): Unit = {
    // Override the configuration of the port when specified as program argument
    val port = if (args.isEmpty) "0" else args(0)
    val config = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").
      withFallback(ConfigFactory.parseString("akka.cluster.roles = [frontend]")).
      withFallback(ConfigFactory.load())

    val system = ActorSystem("ClusterSystem", config)
    val frontend = system.actorOf(Props[PrimFrontend], name = "frontend")

    val counter = new AtomicInteger
    import system.dispatcher
    implicit val timeout = Timeout(5 seconds)
    val graph = UndirectedGraph(5, Edge(0, 1, 0.1),
      Edge(0, 2, 9999.0), Edge(0, 3, 8888.0),
      Edge(1, 4, 1.4), Edge(2, 4, 2.4), Edge(3, 4, 3.4)
    )

    def makeReq(): Unit =
    system.scheduler.scheduleOnce(10.seconds) {
      implicit val timeout = Timeout(5 seconds)

      frontend ? FindMst(graph) onSuccess {
        case jf: JobFailed => println(jf); makeReq()
        case res => println("\n\n\n\nRESULT: " + res)
      }
    }
    makeReq()

  }


}