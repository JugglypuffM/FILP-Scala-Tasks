package exercises06.data

import exercises06.typeclasses._

sealed trait Validated[+E, +A]

object Validated {
  case class Valid[+A](a: A) extends Validated[Nothing, A]

  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  implicit def validatedApplicativeError[E: Semigroup]: ApplicativeError[Validated[E, *], E] =
    new ApplicativeError[Validated[E, *], E] {
      def pure[A](x: A): Validated[E, A] =
        Valid(x)

      def raiseError[A](e: E): Validated[E, A] =
        Invalid(e)

      def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] = {
        (ff, fa) match {
          case (Valid(f), Valid(a))       => Valid(f(a))
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
          case (e @ Invalid(_), _)        => e
          case (_, e @ Invalid(_))        => e
        }
      }

      def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
        fa match {
          case Valid(a)       => Valid(f(a))
          case e @ Invalid(_) => e
        }
    }
}
