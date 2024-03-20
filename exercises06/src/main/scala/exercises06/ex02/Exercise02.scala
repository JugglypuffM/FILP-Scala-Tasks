package exercises06.ex02

import exercises06.data.NonEmptyList
import exercises06.ex02.Domain._
import exercises06.ex02.Errors.ParsingError
import exercises06.typeclasses._

object Exercise02 {
  type TransformationSupport[F[_]] = ApplicativeError[F, NonEmptyList[ParsingError]]

  object TransformationSupport {
    @inline
    def apply[F[_]](implicit inst: TransformationSupport[F]): TransformationSupport[F] =
      inst
  }

  private implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {
    def require[F[_]](err: => ParsingError)(implicit ts: TransformationSupport[F]): F[A] =
      opt match {
        case Some(value) => ts.pure(value)
        case None        => ts.raiseError(NonEmptyList.of(err))
      }
  }

  // Советуем воспользоваться)
  import TupleSyntax._
  import TransformerSyntax._
  import exercises06.ex01.Exercise01.Syntax._
  import exercises06.ex01.Exercise01.Instances._

  implicit def personTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawPerson, Person] = ???

  implicit def addressBookTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawAddressBook, AddressBook] = ???
}
