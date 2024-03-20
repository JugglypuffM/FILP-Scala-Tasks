package examples

import examples.typeclasses.ApplicativeErrorSyntax._
import examples.typeclasses.ApplicativeSyntax._
import examples.typeclasses.{ApplicativeError, Semigroup}

object Example09_ApplicativeError_With_Validated extends App {

  sealed trait Validated[+E, +A]
  object Validated {
    case class Valid[+A](a: A)   extends Validated[Nothing, A]
    case class Invalid[+E](e: E) extends Validated[E, Nothing]
  }

  // Validated[E, *] - ApplicativeError, если E - полугруппа
  implicit def validatedApplicativeError[E: Semigroup]: ApplicativeError[Validated[E, *], E] =
    new ApplicativeError[Validated[E, *], E] {
      override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
        fa match {
          case Validated.Valid(fa)    => Validated.Valid(f(fa))
          case Validated.Invalid(err) => Validated.Invalid(err)
        }

      override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] = {
        import Validated._
        (fa, fb) match {
          case (Valid(a), Valid(b))       => Valid((a, b))
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
          case (e @ Invalid(_), _)        => e
          case (_, e @ Invalid(_))        => e
        }
      }

      override def pure[A](x: A): Validated[E, A] = Validated.Valid(x)

      override def raiseError[A](e: E): Validated[E, A] = Validated.Invalid(e)

      override def handleErrorWith[A](fa: Validated[E, A])(f: E => Validated[E, A]): Validated[E, A] =
        fa match {
          case Validated.Invalid(e) => f(e)
          case other                => other
        }
    }

  // NonEmptyList - контейнер для наших ошибок
  case class NonEmptyList[+A](head: A, tail: List[A] = Nil)

  // NonEmptyList - полугруппа
  implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]] = new Semigroup[NonEmptyList[A]] {
    override def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] = {
      NonEmptyList(x.head, x.tail ::: (y.head :: y.tail))
    }
  }

  type V[+A] = Validated[NonEmptyList[String], A]

  // will compile
  ApplicativeError[V, NonEmptyList[String]]

  // Парсер строк из формы
  trait FromString[A] {
    def parse(str: String): V[A]
  }

  implicit class StringSyntax(private val str: String) extends AnyVal {
    def parse[A](implicit fromString: FromString[A]): V[A] = fromString.parse(str)
  }

  object FromString {
    implicit val idFromString: FromString[String] = Validated.Valid(_)

    implicit val intFromString: FromString[Int] = str =>
      str.toIntOption match {
        case Some(int) => int.pure[V]
        case None      => NonEmptyList(s"Can't parse int from $str").raiseError
      }

    implicit val doubleFromString: FromString[Double] = str =>
      str.toDoubleOption match {
        case Some(double) => double.pure[V]
        case None         => NonEmptyList(s"Can't parse double from $str").raiseError
      }
  }

  case class Person(name: String, age: Int, height: Double)

  val success: V[Person] = (
    "Mike".parse[String],
    "22".parse[Int],
    "187.8".parse[Double]
  ).mapN(Person)
  println(success) // Valid(Person(Mike, 22, 187.8))

  val failed: V[Person] = (
    "Bob".parse[String],
    "lol".parse[Int],
    "kek".parse[Double]
  ).mapN(Person)
  println(failed) // Invalid(NonEmptyList(Can't parse int from lol, List(Can't parse double from kek)))
}
