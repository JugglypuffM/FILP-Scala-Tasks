package registry.domain.model

case class ApprovalEvent(appId: Application.Id, result: ApprovalEvent.Result)

object ApprovalEvent {
  sealed trait Result
  object Result {
    case object Trust    extends Result
    case object Mistrust extends Result
  }
}
