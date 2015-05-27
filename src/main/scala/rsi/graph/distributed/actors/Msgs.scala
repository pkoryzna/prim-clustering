package rsi.graph.distributed.actors

object Msgs {
  case class TransformationJob(text: String)
  case class TransformationResult(text: String)
  case class JobFailed(reason: String, job: TransformationJob)
  case object BackendRegistration

}
