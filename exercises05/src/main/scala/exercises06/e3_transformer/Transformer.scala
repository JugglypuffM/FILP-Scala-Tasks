package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  private def getUserName(rawUser: RawUser): Option[UserName] =
    for {
      firstName  <- rawUser.firstName
      secondName <- rawUser.secondName
    } yield UserName(firstName, secondName, rawUser.thirdName)

  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    def toOption(a: RawUser): Option[User] =
      for {
        id       <- a.id.toLongOption
        username <- getUserName(a)
      } yield User(id, username)

    def toEither(a: RawUser): Either[Error, User] =
      for {
        id       <- a.id.toLongOption.toRight(InvalidId)
        username <- getUserName(a).toRight(InvalidName)
      } yield User(id, username)
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
