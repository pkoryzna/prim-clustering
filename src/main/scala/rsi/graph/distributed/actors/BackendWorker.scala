package rsi.graph.distributed.actors

import akka.actor.{Actor, ActorSystem, Props, RootActorPath}
import akka.cluster.ClusterEvent.{CurrentClusterState, MemberUp}
import akka.cluster.{Cluster, Member, MemberStatus}
import akka.event.LoggingReceive
import com.typesafe.config.ConfigFactory
import rsi.graph.distributed.actors.Msgs._
import rsi.graph.{Prim, PartitionedMatrix, UndirectedGraph}

import scala.language.postfixOps

//#backend
class BackendWorker extends Actor {

  val cluster = Cluster(context.system)

  // subscribe to cluster changes, MemberUp
  // re-subscribe when restart
  override def preStart(): Unit = cluster.subscribe(self, classOf[MemberUp])
  override def postStop(): Unit = cluster.unsubscribe(self)

  def receive = LoggingReceive {
    case FindCheapestInPartition(matrix: PartitionedMatrix, currentPartial: UndirectedGraph, allVertices: Set[Int]) => 
      sender() ! EdgeResult(Prim.cheapestInPartition(matrix, currentPartial, allVertices))
    case state: CurrentClusterState =>
      state.members.filter(_.status == MemberStatus.Up) foreach register
    case MemberUp(m) => register(m)
  }

  def register(member: Member): Unit =
    if (member.hasRole("frontend"))
      context.actorSelection(RootActorPath(member.address) / "user" / "frontend") !
        BackendRegistration
}
//#backend

object BackendWorker {
  def main(args: Array[String]): Unit = {
    // Override the configuration of the port when specified as program argument
    val port = if (args.isEmpty) "0" else args(0)
    val config = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").
      withFallback(ConfigFactory.parseString("akka.cluster.roles = [backend]")).
      withFallback(ConfigFactory.load())

    val system = ActorSystem("ClusterSystem", config)
    system.actorOf(Props[BackendWorker], name = "backend")
  }
}