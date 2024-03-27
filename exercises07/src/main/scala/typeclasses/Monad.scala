package typeclasses

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  def apply[F[_]](implicit inst: Monad[F]): Monad[F] = inst

  object syntax {
    implicit class MonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
      def map[B](f: A => B)(implicit monad: Monad[F]): F[B] =
        monad.map(fa)(f)
      def flatMap[B](f: A => F[B])(implicit monad: Monad[F]): F[B] =
        monad.flatMap(fa)(f)
      def as[B](b: B)(implicit monad: Monad[F]): F[B] =
        monad.map(fa)(_ => b)
    }

    implicit class PureOps[A](private val a: A) extends AnyVal {
      def pure[F[_]](implicit monad: Monad[F]): F[A] =
        monad.pure(a)
    }
  }
}
