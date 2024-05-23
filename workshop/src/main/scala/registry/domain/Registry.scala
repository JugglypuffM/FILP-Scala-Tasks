package registry.domain

import cats._
import cats.effect.Sync
import cats.syntax.all._
import registry.domain.Registry.Error.{UserAlreadyExists, UserApplicationAlreadyExists}
import registry.domain.model.User
import registry.domain.service.{ApplicationAlg, TrustworthinessAlg, UserAlg}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

trait Registry[F[_]] {
  def signUp(user: User): F[Unit]
}

object Registry {
  sealed trait Error extends Exception with NoStackTrace

  object Error {
    case object UserAlreadyExists            extends Error
    case object UserApplicationAlreadyExists extends Error
  }

  //TODO: Реализовать бизнес логику
  def build[F[_]: MonadThrow: Sync](
      userAlg: UserAlg[F],
      appAlg: ApplicationAlg[F],
      trustworthinessAlg: TrustworthinessAlg[F]
  ): Registry[F] = new Registry[F] {
    def signUp(user: User): F[Unit] = {
      for {
        userOption <- userAlg.getBy(user.passport)
        _ <- userOption match {
          case Some(_) => MonadThrow[F].raiseError[Unit](UserAlreadyExists)
          case None    => Sync[F].unit
        }
        appIdOption <- appAlg.getApplicationBy(user)
        _ <- appIdOption match {
          case Some(_) => MonadThrow[F].raiseError[Unit](UserApplicationAlreadyExists)
          case None    => Sync[F].unit
        }
        appId <- trustworthinessAlg.check(user)
        _     <- appAlg.persist(appId, user, FiniteDuration(1, TimeUnit.MINUTES))
      } yield ()
    }
  }
}
