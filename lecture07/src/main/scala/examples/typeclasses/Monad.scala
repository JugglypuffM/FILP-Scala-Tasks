package examples.typeclasses

import scala.util.{Failure, Success, Try}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
}

object Monad {
  @inline def apply[F[_]: Monad]: Monad[F] = implicitly
}

object MonadInstances {

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def pure[A](a: A): List[A]                               = List(a)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def pure[A](a: A): Option[A]                                   = Some(a)
  }

  implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
    override def pure[A](a: A): Either[E, A]                                         = Right(a)
  }

  implicit val tryMonad: Monad[Try] = new Monad[Try] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    override def pure[A](a: A): Try[A]                             = Success(a)
  }
}

object MonadSyntax {

  implicit class MonadOps[F[_]: Monad, A](private val fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
    def map[B](f: A => B): F[B]        = Monad[F].map(fa)(f)
  }

  implicit class PureOps[A](private val a: A) extends AnyVal {
    def pure[F[_]: Monad]: F[A] = Monad[F].pure(a)
  }
}
