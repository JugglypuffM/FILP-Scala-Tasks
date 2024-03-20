package examples.typeclasses

import scala.util.{Failure, Success, Try}

trait ApplicativeError[F[_], E] extends Applicative[F] {

  def raiseError[A](e: E): F[A]

  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
}

object ApplicativeError {
  def apply[F[_], E](implicit ae: ApplicativeError[F, E]): ApplicativeError[F, E] = implicitly
}

object ApplicativeErrorInstances {
  implicit val tryApplicativeError: ApplicativeError[Try, Throwable] = new ApplicativeError[Try, Throwable] {

    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = {
      fa match {
        case Failure(exception) => f(exception)
        case success            => success
      }
    }

    override def pure[A](a: A): Try[A] = Success(a)

    override def product[A, B](fa: Try[A], fb: Try[B]): Try[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
  }

  implicit def eitherApplicativeError[E]: ApplicativeError[Either[E, *], E] = new ApplicativeError[Either[E, *], E] {

    override def raiseError[A](e: E): Either[E, A] = Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] = fa match {
      case Left(error) => f(error)
      case success     => success
    }

    override def pure[A](a: A): Either[E, A] = Right(a)

    override def product[A, B](fa: Either[E, A], fb: Either[E, B]): Either[E, (A, B)] =
      fa.flatMap(a => fb.map(b => (a, b)))

    override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.map(f)
  }
}

object ApplicativeErrorSyntax {

  implicit class ApplicativeErrorRaiseOps[F[_], E](private val e: E) extends AnyVal {
    def raiseError[A](implicit ae: ApplicativeError[F, E]): F[A] =
      ae.raiseError(e)
  }

  implicit class ApplicativeErrorHandleOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def handleErrorWith[E](f: E => F[A])(implicit ae: ApplicativeError[F, E]): F[A] =
      ae.handleErrorWith(fa)(f)
  }
}
