package exercises06.typeclasses

trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
}

object Foldable {
  @inline
  def apply[F[_]: Foldable](implicit inst: Foldable[F]): Foldable[F] =
    inst
}
