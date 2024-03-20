package exercises06.typeclasses

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traverse {
  @inline
  def apply[F[_]: Traverse](implicit inst: Traverse[F]): Traverse[F] =
    inst
}
