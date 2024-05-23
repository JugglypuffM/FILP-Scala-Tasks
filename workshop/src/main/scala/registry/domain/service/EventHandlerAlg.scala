package registry.domain.service

import cats._
import cats.syntax.all._
import registry.domain.model.ApprovalEvent
import registry.domain.service.EventHandlerAlg.Error.UserNotFound

import scala.util.control.NoStackTrace

trait EventHandlerAlg[F[_]] {
  def handle(ae: ApprovalEvent): F[Unit]
}

object EventHandlerAlg {

  sealed trait Error extends Exception with NoStackTrace
  object Error {
    case object UserNotFound extends Error
  }

  def build[F[_]: MonadThrow](userAlg: UserAlg[F], appAlg: ApplicationAlg[F]): EventHandlerAlg[F] =
    new EventHandlerAlgImpl(userAlg, appAlg)

  private class EventHandlerAlgImpl[F[_]: MonadThrow](
      userAlg: UserAlg[F],
      appAlg: ApplicationAlg[F]
  ) extends EventHandlerAlg[F] {

    //TODO: реализовать обработку события
    override def handle(ae: ApprovalEvent): F[Unit] =
      ae match {
        case ApprovalEvent(appId, ApprovalEvent.Result.Trust) =>
          for {
            userOption <- appAlg.getUserBy(ae.appId)
            user       <- userOption.getOrElse(throw UserNotFound).pure
            _          <- userAlg.persist(user)
            _          <- appAlg.remove(appId)
          } yield {}.pure
        case ApprovalEvent(appId, ApprovalEvent.Result.Mistrust) => appAlg.remove(appId)
      }
  }
}
