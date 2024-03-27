package examples.typeclasses

import scala.util.{Failure, Success, Try}

trait MonadError[F[_], E] extends Monad[F] {
  def raiseError[A](e: E): F[A]

  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
}

object MonadError {
  @inline
  def apply[F[_], E](implicit inst: MonadError[F, E]): MonadError[F, E] =
    inst

  object Syntax {
    implicit class HandleErrorOps[F[_], A](private val fa: F[A]) extends AnyVal {
      def handleErrorWith[E](f: E => F[A])(implicit monadError: MonadError[F, E]): F[A] =
        monadError.handleErrorWith(fa)(f)
    }
  }
}

object MonadErrorInstances {
  implicit def eitherMonadError[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    def raiseError[A](e: E): Either[E, A] = Left(e)

    def pure[A](a: A): Either[E, A] = Right(a)

    def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
      fa.left.flatMap(f)

    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa.flatMap(f)
  }

  implicit val tryMonadThrow: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    def raiseError[A](e: Throwable): Try[A] = Failure(e)

    def pure[A](a: A): Try[A] = Success(a)

    def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] =
      fa.recoverWith { case e => f(e) }

    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
  }
}
