package examples.typeclasses

import scala.util.{Failure, Success, Try}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Applicative {
  def apply[F[_]](implicit ev: Applicative[F]): Applicative[F] = implicitly
}

object ApplicativeInstances {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = fa.zip(fb)
  }

  implicit val tryApplicative: Applicative[Try] = new Applicative[Try] {
    override def pure[A](a: A): Try[A] = Success(a)

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)

    override def product[A, B](fa: Try[A], fb: Try[B]): Try[(A, B)] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(a, b)
      case (Failure(a), _)          => Failure(a)
      case (_, Failure(b))          => Failure(b)
    }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {

    override def pure[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }
}

object ApplicativeSyntax {
  implicit class Ops[A](private val a: A) extends AnyVal {
    def pure[F[_]](implicit applicative: Applicative[F]): F[A] = applicative.pure(a)
  }

  implicit class FOps[F[_]: Applicative, A, B](private val fa: F[A]) {
    def product(fb: F[B]): F[(A, B)] = Applicative[F].product(fa, fb)
  }

  implicit class Tuple2Syntax[F[_], A, B](private val tuple2: (F[A], F[B])) extends AnyVal {
    def mapN[Z](f: (A, B) => Z)(implicit a: Applicative[F]): F[Z] = {
      val (fa, fb) = tuple2
      a.map(a.product(fa, fb)) { case (a, b) => f(a, b) }
    }
  }

  implicit class Tuple3Syntax[F[_], A, B, C](private val tuple3: (F[A], F[B], F[C])) extends AnyVal {
    def mapN[Z](f: (A, B, C) => Z)(implicit a: Applicative[F]): F[Z] = {
      val (fa, fb, fc) = tuple3
      a.map(a.product(a.product(fa, fb), fc)) { case ((a, b), c) => f(a, b, c) }
    }
  }
}
