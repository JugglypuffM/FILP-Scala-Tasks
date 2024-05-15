package registry.domain.service

import registry.domain.model.{Application, User}

import scala.concurrent.duration.FiniteDuration

trait ApplicationAlg[F[_]] {
  def getApplicationBy(user: User): F[Option[Application.Id]]
  def getUserBy(app: Application.Id): F[Option[User]]
  def persist(app: Application.Id, user: User, ttl: FiniteDuration): F[Unit]
  def remove(app: Application.Id): F[Unit]
}
