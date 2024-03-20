package exercises06.typeclasses

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  @inline
  def apply[F[_]](implicit inst: Functor[F]): Functor[F] =
    inst
}
