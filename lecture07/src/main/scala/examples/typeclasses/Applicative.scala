package examples.typeclasses

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
}

object Applicative {
  def apply[F[_]](implicit ev: Applicative[F]): Applicative[F] = implicitly
}

object ApplicativeSyntax {
  implicit class Ops[A](private val a: A) extends AnyVal {
    def pure[F[_]](implicit applicative: Applicative[F]): F[A] = applicative.pure(a)
  }
}
