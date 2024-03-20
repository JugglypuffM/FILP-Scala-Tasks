package exercises06.typeclasses

trait ApplicativeError[F[_], E] extends Applicative[F] {
  def raiseError[A](e: E): F[A]
}

object ApplicativeError {
  @inline
  def apply[F[_], E](implicit inst: ApplicativeError[F, E]): ApplicativeError[F, E] =
    inst
}
