package exercises06.typeclasses

trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](x: A): F[A]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)
}

object Applicative {
  @inline
  def apply[F[_]](implicit inst: Applicative[F]): Applicative[F] =
    inst
}
