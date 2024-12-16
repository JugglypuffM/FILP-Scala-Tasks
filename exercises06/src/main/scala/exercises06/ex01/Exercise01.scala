package exercises06.ex01

import exercises06.data.NonEmptyList
import exercises06.typeclasses._
import exercises06.typeclasses.{Applicative, Traverse}

object Exercise01 {
  object Syntax {
    implicit class SemigroupOps[A](private val a: A) extends AnyVal {
      def |+|(b: A)(implicit s: Semigroup[A]): A = s.combine(a, b)
    }
    implicit class ApplicativePure[A](private val a: A) extends AnyVal {
      def pure[F[_]](implicit applicative: Applicative[F]): F[A] = applicative.pure(a)
    }
    implicit class FoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {
      def foldLeft[B](b: B)(f: (B, A) => B)(implicit foldable: Foldable[F]): B =
        foldable.foldLeft[A, B](fa, b)(f)
      def combineAll(implicit foldable: Foldable[F], monoid: Monoid[A]): A =
        fa.foldLeft(Monoid[A].empty)(Monoid[A].combine)
    }
    implicit class ApplicativeOps[F[_], A, B](private val fa: F[A]) {
      def aproduct(fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = Applicative[F].product(fa, fb)
    }
    implicit class TraverseOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
      def traverse[B](f: A => G[B])(implicit a: Applicative[G], t: Traverse[F]): G[F[B]] =
        t.traverse(fa)(f)
    }
    implicit class FunctorOps[F[_], A](private val fa: F[A]) extends AnyVal {
      def map[B](f: A => B)(implicit functor: Functor[F]): F[B] =
        functor.map[A, B](fa)(f)
    }
  }

  object Instances {
    import Syntax._

    implicit val strMonoid: Monoid[String] = new Monoid[String] {
      override def combine(x: String, y: String): String = x + y
      override def empty: String                         = ""
    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def combine(x: Int, y: Int): Int = x + y
      override def empty: Int                   = 0
    }

    implicit val listInstances: Traverse[List] with Applicative[List] = new Traverse[List] with Applicative[List] {
      def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
        fa.foldLeft(List.empty[B].pure[G])((accF, next) =>
          accF.aproduct(f(next)).map {
            case (acc, next) => acc.appended(next)
          }
        )

      def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = fa.zip(ff).map(pair => pair._2(pair._1))

      def pure[A](x: A): List[A] = List(x)

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit val optionInstances: Traverse[Option] with Applicative[Option] =
      new Traverse[Option] with Applicative[Option] {
        def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
          case Some(value) => f(value).map(Some(_))
          case None        => Option.empty[B].pure[G]
        }

        def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = fa.zip(ff).map(pair => pair._2(pair._1))

        def pure[A](x: A): Option[A] = Some(x)

        def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa.fold(b)(f(b, _))
        def map[A, B](fa: Option[A])(f: A => B): Option[B]         = fa.map(f)
      }

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] =
      new Traverse[NonEmptyList] with Applicative[NonEmptyList] {
        def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
          listInstances.traverse(fa.tail)(f).aproduct(f(fa.head)).map({ case (acc, next) => NonEmptyList(next, acc) })

        def ap[A, B](ff: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] =
          NonEmptyList(ff.head(fa.head), fa.tail.zip(ff.tail).map(pair => pair._2(pair._1)))

        def pure[A](x: A): NonEmptyList[A] = NonEmptyList(x)

        def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B = (fa.head :: fa.tail).foldLeft(b)(f)

        def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = NonEmptyList(f(fa.head), fa.tail.map(f))
      }

    implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def combine(x: List[A], y: List[A]): List[A] = x ::: y
      override def empty: List[A]                           = Nil
    }
  }
}
