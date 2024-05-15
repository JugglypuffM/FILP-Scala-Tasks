package registry.domain.service

import registry.domain.model.User
import registry.domain.model.Application

trait TrustworthinessAlg[F[_]] {
  def check(user: User): F[Application.Id]
}
