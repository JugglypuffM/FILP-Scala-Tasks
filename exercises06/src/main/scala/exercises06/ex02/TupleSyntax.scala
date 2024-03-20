package exercises06.ex02

import exercises06.typeclasses.Applicative

object TupleSyntax {
  implicit class Tuple2Syntax[F[_], A, B](private val tuple: Tuple2[F[A], F[B]]) extends AnyVal {
    def mapN[Z](f: (A, B) => Z)(implicit applicative: Applicative[F]): F[Z] =
      applicative.map(applicative.product(tuple._1, tuple._2)) { case (a, b) => f(a, b) }
  }

  implicit class Tuple3Syntax[F[_], A, B, C](private val tuple: (F[A], F[B], F[C])) extends AnyVal {
    def mapN[Z](f: (A, B, C) => Z)(implicit applicative: Applicative[F]): F[Z] =
      applicative.map(applicative.product(applicative.product(tuple._1, tuple._2), tuple._3)) {
        case ((a, b), c) => f(a, b, c)
      }
  }
}
