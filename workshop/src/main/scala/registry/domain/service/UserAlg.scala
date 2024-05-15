package registry.domain.service

import registry.domain.model.{Passport, User}

trait UserAlg[F[_]] {
  def getBy(passport: Passport): F[Option[User]]
  def persist(user: User): F[Unit]
}
