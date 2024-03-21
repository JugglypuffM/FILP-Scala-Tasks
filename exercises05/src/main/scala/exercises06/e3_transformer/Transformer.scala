package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    def toOption(a: RawUser): Option[User] = (a.id.toLongOption, a.firstName, a.secondName) match {
      case (Some(id), Some(first), Some(second)) => Some(User(id, UserName(first, second, a.thirdName)))
      case _                                     => None
    }

    def toEither(a: RawUser): Either[Error, User] = (a.id.toLongOption, a.firstName, a.secondName) match {
      case (Some(id), Some(first), Some(second)) => Right(User(id, UserName(first, second, a.thirdName)))
      case (None, _, _)                          => Left(InvalidId)
      case _                                     => Left(InvalidName)
    }
  }
}

object TransformerSyntax {
  implicit class TransformerOps[A](val value: A) extends AnyVal {
    def transformToOption[B]()(implicit transformer: Transformer[A, B]): Option[B]        = transformer.toOption(value)
    def transformToEither[B]()(implicit transformer: Transformer[A, B]): Either[Error, B] = transformer.toEither(value)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
