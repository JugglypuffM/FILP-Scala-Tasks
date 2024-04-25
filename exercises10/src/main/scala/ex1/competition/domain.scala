package ex1.competition

object domain {
  sealed trait ScenarioError extends Throwable
  object ScenarioError {
    case object TopAuthorNotFound extends ScenarioError
  }
}
