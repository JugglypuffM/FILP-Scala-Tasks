package registry.domain.service

trait EventDispatcherAlg[F[_]] {
  def start(eha: EventHandlerAlg[F]): F[Unit]
}
